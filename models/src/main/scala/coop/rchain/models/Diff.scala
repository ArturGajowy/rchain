package coop.rchain.models
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.models.Diff.R
import monix.eval.Coeval

import scala.collection.immutable.{BitSet, HashSet}

trait Diff[A] {

  def diff(x: A, y: A): R[Unit]

}

object Diff extends LowPriorityGenericDiff {
  type R[A] = Either[(Any, Any), A]

  def apply[A](implicit ev: Diff[A]) = ev

  implicit val StringDiff         = fromEquals[String]
  implicit val LongDiff           = fromEquals[Long]
  implicit val IntDiff            = fromEquals[Int]
  implicit val BitSetDiff         = fromEquals[BitSet]
  implicit val ThrowableDiff      = fromEquals[Throwable]
  implicit val BooleanDiff        = fromEquals[Boolean]
  implicit val ByteStringDiff     = fromEquals[ByteString]
  implicit def AlwaysEqualDiff[A] = fromEquals[AlwaysEqual[A]]

  implicit def coevalDiff[A: Diff]: Diff[Coeval[A]] =
    (x, y) => Diff[A].diff(x.value, y.value) // Diff.on

  def fromEquals[A]: Diff[A] = (x: A, y: A) => check(x == y, x, y)

  def check[T](condition: => Boolean, x: T, y: T): R[Unit] =
    if (condition) Right(()) else Left(x -> y)

  implicit val parDiff  = gen[Par]
  implicit val ExprDiff = gen[Expr]
  implicit val sendDiff = gen[Send]

  implicit def seqDiff[A: Diff]: Diff[Seq[A]]             = iterableDiff[Seq[A], A]
  implicit def hashSetDiff[A: Diff]: Diff[HashSet[A]]     = iterableDiff[HashSet[A], A]
  implicit def mapDiff[A: Diff, B: Diff]: Diff[Map[A, B]] = iterableDiff[Map[A, B], (A, B)]

  /*implicit*/
  def iterableDiff[F <: Iterable[A], A: Diff]: Diff[F] = new Diff[F] {
    override def diff(x: F, y: F): R[Unit] =
      x.zip(y).toList.traverse_[R, Unit](pair => Diff[A].diff(pair._1, pair._2))
  }

//  def anyDiff[A]: Diff[A] = new Diff[A] {
//    override def diff(x: A, y: A): R[Unit] = check(x == y, x, y)
//  }
}

trait LowPriorityGenericDiff extends DiffDerivation {
//  def anyDiff[A]: Diff[A] = new Diff[A] {
//    override def diff(x: A, y: A): R[Unit] = check(x == y, x, y)
//  }
}

trait DiffDerivation {
  import language.experimental.macros
  import magnolia._
  import Diff.check

  type Typeclass[T] = Diff[T]

  def combine[T](ctx: CaseClass[Diff, T]): Diff[T] = new Diff[T] {
    def diff(x: T, y: T): R[Unit] =
      for {
        _ <- ctx.parameters.toList.traverse[R, Unit] { p =>
              p.typeclass.diff(p.dereference(x), p.dereference(y))
            }
        _ <- check(x == y, x, y)
      } yield ()
  }

  def dispatch[T](ctx: SealedTrait[Diff, T]): Diff[T] =
    new Diff[T] {
      def diff(x: T, y: T): R[Unit] =
        ctx.dispatch(x) { sub =>
          check(sub.cast.isDefinedAt(y), x, y)
            .flatMap(_ => sub.typeclass.diff(sub.cast(x), sub.cast(y)))
        }
    }

  implicit def gen[T]: Diff[T] = macro Magnolia.gen[T]
}
