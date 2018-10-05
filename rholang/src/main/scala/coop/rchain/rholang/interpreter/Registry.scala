package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import com.google.protobuf.ByteString.ByteIterator
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Registry.FixedRefs._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.util._
import org.lightningj.util.ZBase32

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.{Seq => RootSeq}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Registry implements a radix tree for public lookup of one-sided bundles.
  * The radix tree is implemented as follows:
  * Nodes are maps.
  * The keys in the map are byte arrays of length [0, 1]
  * The values in the map are tuples:
  * (tag, edgeAdditional, data)
  * tag:
  * if tag is 0, it is a terminal edge (We absorb leaf nodes into their parent)
  * if tag is 1, data is a name where another map can be read.
  * edgeAdditional:
  * this is a bytestring that stores any additional edge labeling in the radix tree.
  * data:
  * if tag is 0, this is the stored data.
  * if tag is 1, this is a name where the process recurs.
  */
trait Registry[F[_]] {

  def testInstall(): F[Unit]

  def lookup(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def lookupCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def insert(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def insertCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def delete(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def deleteRootCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def deleteCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def publicLookup(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def publicRegisterRandom(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]

  def publicRegisterInsertCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit]
}

class RegistryImpl[F[_]](
    private val space: Runtime.RhoPureSpace[F],
    private val dispatcher: Runtime.RhoDispatch[F]
)(implicit F: Sync[F])
    extends Registry[F] {

  import Registry._
  private def commonPrefix(b1: ByteString, b2: ByteString): ByteString = {
    val prefixOut = ByteString.newOutput()
    @tailrec
    def loop(it1: ByteIterator, it2: ByteIterator): ByteString =
      if (!it1.hasNext || !it2.hasNext) {
        prefixOut.toByteString
      } else {
        val b: Byte = it1.nextByte
        if (it2.nextByte == b) {
          prefixOut.write(b.toInt)
          loop(it1, it2)
        } else {
          prefixOut.toByteString()
        }
      }
    loop(b1.iterator, b2.iterator)
  }

  private def safeUncons(b: ByteString): (ByteString, ByteString) = {
    val head = if (b.isEmpty()) b else b.substring(0, 1)
    val tail = if (b.isEmpty()) b else b.substring(1)
    (head, tail)
  }

  private val lookupPatterns = List(
    BindPattern(
      Seq(
        Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
        Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)
      ),
      freeCount = 2
    )
  )
  // Testing only
  private val lookupChannels  = List[Par](GPrivate(ByteString.copyFrom(Array[Byte](10))))
  private val insertRef: Long = Runtime.BodyRefs.REG_INSERT
  private val insertPatterns = List(
    BindPattern(
      Seq(
        Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
        Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true),
        Par(exprs = Seq(EVar(FreeVar(2))), connectiveUsed = true)
      ),
      freeCount = 3
    )
  )
  // Testing only
  private val insertChannels = List[Par](GPrivate(ByteString.copyFrom(Array[Byte](12))))
  private val deletePatterns = List(
    BindPattern(
      Seq(
        Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
        Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)
      ),
      freeCount = 2
    )
  )
  // Testing only
  private val deleteChannels = List[Par](GPrivate(ByteString.copyFrom(Array[Byte](14))))

  private val publicLookupRef: Long = Runtime.BodyRefs.REG_PUBLIC_LOOKUP
  // Testing only
  private val publicLookupChannels = List[Par](GPrivate(ByteString.copyFrom(Array[Byte](17))))
  private val publicLookupPatterns = List(
    BindPattern(
      Seq(
        Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
        Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)
      ),
      freeCount = 2
    )
  )

  private val publicRegisterRandomRef: Long = Runtime.BodyRefs.REG_PUBLIC_REGISTER_RANDOM
  private val publicRegisterInsertCallbackRef: Long =
    Runtime.BodyRefs.REG_PUBLIC_REGISTER_INSERT_CALLBACK
  // Testing only
  private val publicRegisterRandomChannels =
    List[Par](GPrivate(ByteString.copyFrom(Array[Byte](18))))
  private val publicRegisterRandomPatterns = List(
    BindPattern(
      Seq(
        Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
        Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)
      ),
      freeCount = 2
    )
  )

  private val publicRegisterInsertCallbackPatterns = List(
    BindPattern(
      Seq(
        Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
        Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true),
        Par(exprs = Seq(EVar(FreeVar(2))), connectiveUsed = true)
      ),
      freeCount = 3
    ),
    BindPattern(
      Seq(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
      freeCount = 1
    )
  )

  def testInstall(): F[Unit] =
    for {
      _ <- space.install(
            lookupChannels,
            lookupPatterns,
            TaggedContinuation(ScalaBodyRef(lookupRef))
          )
      _ <- space.install(
            insertChannels,
            insertPatterns,
            TaggedContinuation(ScalaBodyRef(insertRef))
          )
      _ <- space.install(
            deleteChannels,
            deletePatterns,
            TaggedContinuation(ScalaBodyRef(deleteRef))
          )
      _ <- space.install(
            publicLookupChannels,
            publicLookupPatterns,
            TaggedContinuation(ScalaBodyRef(publicLookupRef))
          )
      _ <- space.install(
            publicRegisterRandomChannels,
            publicRegisterRandomPatterns,
            TaggedContinuation(ScalaBodyRef(publicRegisterRandomRef))
          )
    } yield Unit

  private val prefixRetReplacePattern = BindPattern(
    Seq(
      Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
      Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true),
      Par(exprs = Seq(EVar(FreeVar(2))), connectiveUsed = true)
    ),
    freeCount = 3
  )
  private val prefixValueRetReplacePattern = BindPattern(
    Seq(
      Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
      Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true),
      Par(exprs = Seq(EVar(FreeVar(2))), connectiveUsed = true),
      Par(exprs = Seq(EVar(FreeVar(3))), connectiveUsed = true)
    ),
    freeCount = 4
  )
  private val parentKeyDataReplacePattern = BindPattern(
    Seq(
      Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true),
      Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true),
      Par(exprs = Seq(EVar(FreeVar(2))), connectiveUsed = true)
    ),
    freeCount = 3
  )
  private val triePattern = BindPattern(
    Seq(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
    freeCount = 1
  )

  private def parByteArray(bs: ByteString): Par = GByteArray(bs)

  private def handleResult[E <: Throwable](
      resultF: F[Either[E, Option[(TaggedContinuation, Seq[ListParWithRandomAndPhlos])]]]
  ): F[Unit] =
    resultF.flatMap({
      case Right(Some((continuation, dataList))) => dispatcher.dispatch(continuation, dataList)
      case Right(None)                           => F.unit
      case Left(err)                             => F.raiseError(err)
    })

  private def singleSend(data: Par, chan: Par, rand: Blake2b512Random): F[Unit] =
    handleResult(space.produce(chan, ListParWithRandom(Seq(data), rand), false))

  private def succeed(result: Par, ret: Par, rand: Blake2b512Random): F[Unit] =
    singleSend(result, ret, rand)

  private def fail(ret: Par, rand: Blake2b512Random): F[Unit] =
    singleSend(Par(), ret, rand)

  private def replace(data: Par, replaceChan: Par, dataRand: Blake2b512Random): F[Unit] =
    singleSend(data, replaceChan, dataRand)

  private def failAndReplace(
      data: Par,
      replaceChan: Par,
      retChan: Par,
      dataRand: Blake2b512Random,
      failRand: Blake2b512Random
  ): F[Unit] =
    for {
      _ <- replace(data, replaceChan, dataRand)
      _ <- fail(retChan, failRand)
    } yield ()

  private def fetchDataLookup(
      dataSource: Par,
      key: Par,
      ret: Par,
      rand: Blake2b512Random
  ): F[Unit] = {
    val channel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              channel,
              ListParWithRandom(Seq(key, ret, dataSource), rand),
              false
            )
          )
      _ <- handleResult(
            space.consume(
              Seq[Par](channel, dataSource),
              Seq(prefixRetReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(lookupCallbackRef)),
              false
            )
          )
    } yield ()
  }

  private def fetchDataInsert(
      dataSource: Par,
      key: Par,
      value: Par,
      ret: Par,
      rand: Blake2b512Random
  ): F[Unit] = {
    val channel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              channel,
              ListParWithRandom(Seq(key, value, ret, dataSource), rand),
              false
            )
          )
      _ <- handleResult(
            space.consume(
              Seq[Par](channel, dataSource),
              Seq(prefixValueRetReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(insertCallbackRef)),
              false
            )
          )
    } yield ()
  }

  private def fetchDataRootDelete(
      dataSource: Par,
      key: Par,
      ret: Par,
      rand: Blake2b512Random
  ): F[Unit] = {
    val channel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              channel,
              ListParWithRandom(Seq(key, ret, dataSource), rand),
              false
            )
          )
      _ <- handleResult(
            space.consume(
              Seq[Par](channel, dataSource),
              Seq(prefixRetReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(deleteRootCallbackRef)),
              false
            )
          )
    } yield ()
  }

  private def fetchDataDelete(
      dataSource: Par,
      key: Par,
      ret: Par,
      rand: Blake2b512Random,
      parentKey: Par,
      parentData: Par,
      parentReplace: Par,
      parentRand: Blake2b512Random
  ): F[Unit] = {
    val keyChannel: Par    = GPrivate(ByteString.copyFrom(rand.next()))
    val parentChannel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              keyChannel,
              ListParWithRandom(Seq(key, ret, dataSource), rand),
              false
            )
          )
      _ <- handleResult(
            space.produce(
              parentChannel,
              ListParWithRandom(Seq(parentKey, parentData, parentReplace), parentRand),
              false
            )
          )
      _ <- handleResult(
            space.consume(
              Seq[Par](keyChannel, parentChannel, dataSource),
              Seq(prefixRetReplacePattern, parentKeyDataReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(deleteCallbackRef)),
              false
            )
          )
    } yield ()
  }

  def lookup(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(ListParWithRandomAndPhlos(Seq(key, ret), rand, cost)) =>
        try {
          val Some(Expr(GByteArray(_))) = key.singleExpr
          fetchDataLookup(registryRoot, key, ret, rand)
        } catch {
          case _: MatchError => fail(ret, rand)
        }
      case _ => F.unit
    }

  // A lookup result should result in 1 of 3 things
  // Result not there. Return Nil
  // Result there, return it.
  // Further lookup needed, recurse.

  def lookupCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(
          ListParWithRandomAndPhlos(Seq(key, ret, replaceChan), callRand, callCost),
          ListParWithRandomAndPhlos(Seq(data), dataRand, dataCost)
          ) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        try {
          val Some(Expr(GByteArray(bs)))               = key.singleExpr
          val (head, tail)                             = safeUncons(bs)
          val Some(Expr(EMapBody(parMap)))             = data.singleExpr
          val Some(value)                              = parMap.ps.get(Expr(GByteArray(head)))
          val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = value.singleExpr()
          if (ps.length != 3) {
            localFail()
          } else {
            // The second tuple field should be a bytearray in both cases.
            val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
            ps(0).singleExpr() match {
              case Some(Expr(GInt(0))) =>
                if (tail == edgeAdditional)
                  replace(data, replaceChan, dataRand).flatMap(_ => succeed(ps(2), ret, callRand))
                else
                  localFail()
              case Some(Expr(GInt(1))) =>
                if (tail.startsWith(edgeAdditional)) {
                  val newKey = tail.substring(edgeAdditional.size)

                  replace(data, replaceChan, dataRand).flatMap(
                    _ =>
                      fetchDataLookup(
                        ps(2),
                        parByteArray(newKey),
                        ret,
                        callRand
                      )
                  )
                } else
                  localFail()
            }
          }
        } catch {
          case _: MatchError => localFail()
        }
      case _ => F.unit
    }

  def insert(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(ListParWithRandomAndPhlos(Seq(key, value, ret), rand, cost)) =>
        try {
          val Some(Expr(GByteArray(_))) = key.singleExpr
          fetchDataInsert(registryRoot, key, value, ret, rand)
        } catch {
          case _: MatchError => fail(ret, rand)
        }
      case _ => F.unit
    }

  def insertCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(
          ListParWithRandomAndPhlos(Seq(key, value, ret, replaceChan), callRand, callCost),
          ListParWithRandomAndPhlos(Seq(data), dataRand, dataCost)
          ) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        try {
          val Some(Expr(GByteArray(bs)))   = key.singleExpr
          val (head, tail)                 = safeUncons(bs)
          val Some(Expr(EMapBody(parMap))) = data.singleExpr
          def insert() = {
            val tuple: Par  = ETuple(Seq(GInt(0), parByteArray(tail), value))
            val newMap: Par = ParMap(SortedParMap(parMap.ps + (parByteArray(head) -> tuple)))
            replace(newMap, replaceChan, dataRand)
              .flatMap(_ => succeed(value, ret, callRand))
          }
          parMap.ps.get(parByteArray(head)) match {
            case None => insert()
            case Some(mapEntry) =>
              val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = mapEntry.singleExpr()
              if (ps.length != 3)
                localFail()
              else {
                // The second tuple field should be a bytearray in both cases.
                val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
                def split() = {
                  val outgoingEdgeStr: ByteString = commonPrefix(edgeAdditional, tail)
                  val outgoingEdge: Par           = parByteArray(outgoingEdgeStr)
                  val oldEdgeStr: ByteString      = edgeAdditional.substring(outgoingEdgeStr.size())
                  val newEdgeStr: ByteString      = tail.substring(outgoingEdgeStr.size())
                  val (oldEdgeHead, oldEdgeTail)  = safeUncons(oldEdgeStr)
                  val (newEdgeHead, newEdgeTail)  = safeUncons(newEdgeStr)
                  val newMap: ParMap = ParMap(
                    SortedParMap(
                      Seq[(Par, Par)](
                        parByteArray(oldEdgeHead) -> ETuple(
                          Seq(ps(0), parByteArray(oldEdgeTail), ps(2))
                        ),
                        parByteArray(newEdgeHead) -> ETuple(
                          Seq(GInt(0), parByteArray(newEdgeTail), value)
                        )
                      )
                    )
                  )
                  val newName: Par      = GPrivate(ByteString.copyFrom(callRand.next()))
                  val updatedTuple: Par = ETuple(Seq(GInt(1), outgoingEdge, newName))
                  val updatedMap: Par = ParMap(
                    SortedParMap(parMap.ps + (parByteArray(head) -> updatedTuple))
                  )
                  for {
                    _ <- replace(updatedMap, replaceChan, dataRand)
                    _ <- replace(newMap, newName, callRand.splitByte(0))
                    _ <- succeed(value, ret, callRand.splitByte(1))
                  } yield ()
                }
                ps(0).singleExpr() match {
                  case Some(Expr(GInt(0))) =>
                    // Replace key
                    if (tail == edgeAdditional) {
                      insert()
                    } else {
                      split()
                    }
                  case Some(Expr(GInt(1))) =>
                    // If we have a complete match, recurse.
                    if (tail.startsWith(edgeAdditional)) {
                      val newKey = tail.substring(edgeAdditional.size)

                      replace(data, replaceChan, dataRand).flatMap(
                        _ =>
                          fetchDataInsert(
                            ps(2),
                            parByteArray(newKey),
                            value,
                            ret,
                            callRand
                          )
                      )
                    } else {
                      split()
                    }
                }
              }
          }
        } catch {
          case _: MatchError => localFail()
        }
      case _ => F.unit
    }

  def delete(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(ListParWithRandomAndPhlos(Seq(key, ret), rand, cost)) =>
        try {
          val Some(Expr(GByteArray(_))) = key.singleExpr
          fetchDataRootDelete(registryRoot, key, ret, rand)
        } catch {
          case _: MatchError => fail(ret, rand)
        }
      case _ => F.unit
    }

  def deleteRootCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(
          ListParWithRandomAndPhlos(Seq(key, ret, replaceChan), callRand, callCost),
          ListParWithRandomAndPhlos(Seq(data), dataRand, dataCost)
          ) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        try {
          val Some(Expr(GByteArray(bs)))               = key.singleExpr
          val (head, tail)                             = safeUncons(bs)
          val Some(Expr(EMapBody(parMap)))             = data.singleExpr
          val Some(value)                              = parMap.ps.get(parByteArray(head))
          val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = value.singleExpr()
          if (ps.length != 3)
            localFail()
          else {
            // The second tuple field should be a bytearray in both cases.
            val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
            ps(0).singleExpr() match {
              case Some(Expr(GInt(0))) =>
                if (tail == edgeAdditional) {
                  val updatedMap: Par = ParMap(SortedParMap(parMap.ps - parByteArray(head)))
                  replace(updatedMap, replaceChan, dataRand)
                    .flatMap(_ => succeed(ps(2), ret, callRand))
                } else {
                  localFail()
                }
              case Some(Expr(GInt(1))) =>
                if (tail.startsWith(edgeAdditional)) {
                  val newKey = tail.substring(edgeAdditional.size)
                  fetchDataDelete(
                    ps(2),
                    parByteArray(newKey),
                    ret,
                    callRand,
                    parByteArray(head),
                    data,
                    replaceChan,
                    dataRand
                  )
                } else {
                  localFail()
                }
            }
          }
        } catch {
          case _: MatchError => localFail()
        }
      case _ => F.unit
    }

  def deleteCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(
          ListParWithRandomAndPhlos(Seq(key, ret, replaceChan), callRand, callCost),
          ListParWithRandomAndPhlos(
            Seq(parentKey, parentData, parentReplace),
            parentRand,
            parentCost
          ),
          ListParWithRandomAndPhlos(Seq(data), dataRand, dataCost)
          ) =>
        def localFail() =
          replace(parentData, parentReplace, parentRand).flatMap(
            _ => failAndReplace(data, replaceChan, ret, dataRand, callRand)
          )
        try {
          val Some(Expr(GByteArray(bs))) = key.singleExpr
          val (head, tail)               = safeUncons(bs)

          def mergeWithParent(lastKey: Par, lastEntry: Par): F[Unit] = {
            val Some(Expr(EMapBody(parMap)))                   = parentData.singleExpr()
            val Some(parentEntry)                              = parMap.ps.get(parentKey)
            val Some(Expr(ETupleBody(ETuple(parentPs, _, _)))) = parentEntry.singleExpr()
            if (parentPs.length != 3)
              localFail()
            else {
              val Some(Expr(GByteArray(parentAdditional))) = parentPs(1).singleExpr
              val Some(Expr(GByteArray(lastKeyStr)))       = lastKey.singleExpr()
              val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = lastEntry.singleExpr()
              if (ps.length != 3)
                localFail()
              else {
                val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
                val mergeStream                            = ByteString.newOutput()
                parentAdditional.writeTo(mergeStream)
                lastKeyStr.writeTo(mergeStream)
                edgeAdditional.writeTo(mergeStream)
                val mergedEdge        = parByteArray(mergeStream.toByteString())
                val updatedTuple: Par = ETuple(Seq(ps(0), mergedEdge, ps(2)))
                val updatedMap: Par = ParMap(
                  SortedParMap(parMap.ps + (parentKey -> updatedTuple))
                )
                replace(updatedMap, parentReplace, parentRand)
              }
            }
          }

          val Some(Expr(EMapBody(parMap)))             = data.singleExpr
          val Some(value)                              = parMap.ps.get(parByteArray(head))
          val Some(Expr(ETupleBody(ETuple(ps, _, _)))) = value.singleExpr()
          if (ps.length != 3)
            localFail()
          else {
            // The second tuple field should be a bytearray in both cases.
            val Some(Expr(GByteArray(edgeAdditional))) = ps(1).singleExpr()
            ps(0).singleExpr() match {
              case Some(Expr(GInt(0))) =>
                if (tail == edgeAdditional) {
                  if (parMap.ps.size > 2) {
                    val updatedMap: Par = ParMap(SortedParMap(parMap.ps - parByteArray(head)))
                    for {
                      _ <- replace(updatedMap, replaceChan, dataRand)
                      _ <- replace(parentData, parentReplace, parentRand)
                      _ <- succeed(ps(2), ret, callRand)
                    } yield ()
                  } else if (parMap.ps.size != 2) {
                    localFail()
                  } else {
                    val shrunkMap = (parMap.ps - parByteArray(head))
                    for {
                      _ <- Function.tupled(mergeWithParent(_, _))(shrunkMap.head)
                      _ <- succeed(ps(2), ret, callRand)
                    } yield ()
                  }
                } else {
                  localFail()
                }
              case Some(Expr(GInt(1))) =>
                if (tail.startsWith(edgeAdditional)) {
                  val newKey = tail.substring(edgeAdditional.size)
                  fetchDataDelete(
                    ps(2),
                    parByteArray(newKey),
                    ret,
                    callRand,
                    parByteArray(head),
                    data,
                    replaceChan,
                    dataRand
                  )
                } else {
                  localFail()
                }
            }
          }
        } catch {
          case _: MatchError => localFail()
        }
      case _ => F.unit
    }

  def publicLookup(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(ListParWithRandomAndPhlos(Seq(key, ret), rand, cost)) =>
        def localFail() = fail(ret, rand)
        try {
          val Some(Expr(GUri(uri))) = key.singleExpr
          if (uri.startsWith("rho:id:")) {
            val tail = uri.substring("rho:id:".length)
            if (tail.size != 54) {
              localFail()
            } else {
              // Could fail
              // 256 bits plus 14 bit crc-14
              val bytes: Array[Byte] = ZBase32.decode(tail, 270)
              val crc: Short =
                ((bytes(32).toShort & 0xff) | ((bytes(33).toShort & 0xfc) << 6)).toShort
              if (crc == CRC14.compute(bytes.view.slice(0, 32))) {
                val args = RootSeq(
                  ListParWithRandomAndPhlos(
                    Seq(parByteArray(ByteString.copyFrom(bytes, 0, 32)), ret),
                    rand
                  )
                )
                lookup(args)
              } else {
                localFail()
              }
            }
          } else {
            localFail()
          }
        } catch {
          case _: MatchError               => localFail()
          case _: IllegalArgumentException => localFail()
        }
      case _ => F.unit
    }

  def publicRegisterRandom(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(ListParWithRandomAndPhlos(Seq(value, ret), rand, cost)) =>
        def localFail() = fail(ret, rand)
        try {
          if (value.serializedSize > 1024)
            localFail()
          else {
            val bytes           = rand.next()
            val partialKey: Par = parByteArray(ByteString.copyFrom(bytes))
            val curryChan: Par  = GPrivate(ByteString.copyFrom(rand.next()))
            val resultChan: Par = GPrivate(ByteString.copyFrom(rand.next()))
            val uri: Par        = GUri(buildURI(bytes))
            val args = RootSeq(
              ListParWithRandomAndPhlos(Seq(partialKey, value, resultChan), rand)
            )
            for {
              _ <- handleResult(
                    space.produce(
                      curryChan,
                      // This re-use of rand is fine because we throw it away in the callback below.
                      ListParWithRandom(Seq(uri, value, ret), rand),
                      false
                    )
                  )
              _ <- handleResult(
                    space.consume(
                      Seq[Par](curryChan, resultChan),
                      publicRegisterInsertCallbackPatterns,
                      TaggedContinuation(ScalaBodyRef(publicRegisterInsertCallbackRef)),
                      false
                    )
                  )
              _ <- insert(args)
            } yield ()
          }
        } catch {
          case _: MatchError               => localFail()
          case _: IllegalArgumentException => localFail()
        }
      case _ => F.unit
    }

  def publicRegisterInsertCallback(args: RootSeq[ListParWithRandomAndPhlos]): F[Unit] =
    args match {
      case Seq(
          ListParWithRandomAndPhlos(Seq(urn, expectedValue, ret), _, callCost),
          ListParWithRandomAndPhlos(Seq(value), valRand, valCost)
          ) =>
        if (expectedValue == value)
          singleSend(urn, ret, valRand)
        else
          fail(ret, valRand)
      case _ =>
        F.unit
    }
}

object Registry {
  val registryRoot = GPrivate(
    ByteString
      .copyFrom(Base16.decode("a4fd447dedfc960485983ee817632cf36d79f45fd1796019edfb4a84a81d1697"))
  )

  val emptyMap: Par = EMapBody(ParMap(SortedParMap.empty))

  def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

  val testingUrnMap: Map[String, Par] = Map(
    "rho:registry:testing:lookup"  -> byteName(10),
    "rho:registry:testing:insert"  -> byteName(12),
    "rho:registry:testing:delete"  -> byteName(14),
    "rho:registry:lookup"          -> byteName(17),
    "rho:registry:insertArbitrary" -> byteName(18)
  )

  object FixedRefs {
    val lookupRef: Long               = Runtime.BodyRefs.REG_LOOKUP
    val lookupCallbackRef: Long       = Runtime.BodyRefs.REG_LOOKUP_CALLBACK
    val insertRef: Long               = Runtime.BodyRefs.REG_INSERT
    val deleteRef: Long               = Runtime.BodyRefs.REG_DELETE
    val insertCallbackRef: Long       = Runtime.BodyRefs.REG_INSERT_CALLBACK
    val deleteRootCallbackRef: Long   = Runtime.BodyRefs.REG_DELETE_ROOT_CALLBACK
    val deleteCallbackRef: Long       = Runtime.BodyRefs.REG_DELETE_CALLBACK
    val publicLookupRef: Long         = Runtime.BodyRefs.REG_PUBLIC_LOOKUP
    val publicRegisterRandomRef: Long = Runtime.BodyRefs.REG_PUBLIC_REGISTER_RANDOM
    val publicRegisterInsertCallbackRef: Long =
      Runtime.BodyRefs.REG_PUBLIC_REGISTER_INSERT_CALLBACK

  }

  object CRC14 {
    val INIT_REMAINDER: Short = 0
    def update(rem: Short, b: Byte): Short = {
      @tailrec
      def loop(i: Int, rem: Short): Short =
        if (i < 8) {
          val shiftRem: Short = (rem << 1).toShort
          if ((shiftRem & 0x4000) != 0)
            loop(i + 1, (shiftRem ^ 0x4805).toShort)
          else
            loop(i + 1, shiftRem)
        } else {
          rem
        }
      loop(0, (rem ^ (b << 6).toShort).toShort)
    }

    def compute(b: IndexedSeq[Byte]) =
      b.foldLeft(INIT_REMAINDER)(update(_, _))
  }

  def buildURI(arr: Array[Byte]): String = {
    val fullKey = new Array[Byte](34)
    Array.copy(arr, 0, fullKey, 0, 32)
    val crc = CRC14.compute(fullKey.view.slice(0, 32))
    fullKey(32) = (crc & 0xff).toByte
    fullKey(33) = ((crc & 0xff00) >>> 6).toByte
    "rho:id:" + ZBase32.encodeToString(fullKey, 270)
  }
}