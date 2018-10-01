package coop.rchain.models

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import coop.rchain
import coop.rchain.models
import matryoshka._
import matryoshka.implicits._
import scalapb.GeneratedMessage
import scalaz.{Applicative, Free, Functor, Monad, Traverse}
import scalaz.Scalaz._
import Functor._

import scala.language.higherKinds

object Proto {

  type GenMsg = GeneratedMessage
  type Size   = Int
  type In     = CodedInputStream
  type Out    = CodedOutputStream

//  type Base[A] = Message[A]
//  implicit val BaseFunctor = Message.MessageFunctor
//  implicit val BaseTraverse = Message.MessageTraverse

  sealed trait Descriptor {}

  sealed trait Value[A]
  case class Message[A](fields: List[Field[A]] /* d: Descriptor*/ ) extends Value[A]
  case class Scalar[A](value: ScalarValue)                          extends Value[A]

  sealed trait Field[A]
  case class RequiredField[A](value: A)            extends Field[A]
  case class OptionalField[A](valueOpt: Option[A]) extends Field[A]
  case class RepeatedField[A](values: List[A])     extends Field[A]

  trait ScalarValue {
    type T
    val value: T
    def read[M[_]: Monad](in: In): M[T]
    def write[M[_]: Monad](out: Out): M[Unit]
    def hash(): Int
  }

  case class IntValue(value: Int) extends ScalarValue {
    type T = Int
    def read[M[_]: Monad](in: In): M[T]       = ???
    def write[M[_]: Monad](out: Out): M[Unit] = ???
    override def hash(): Int                  = value.hashCode()
  }

  case class IntValue(value: BitSet) extends ScalarValue {
    type T = BitSet
    def read[M[_]: Monad](in: In): M[T]       = ???
    def write[M[_]: Monad](out: Out): M[Unit] = ???
    override def hash(): BitSet               = value.hashCode()
  }

  object Value {

    //FIXME provide impl
    implicit val ValueFunctor: Functor[Value] = new Functor[Value] {
      override def map[A, B](fa: Value[A])(f: A => B): Value[B] = new Value[B] {}
    }

    //FIXME provide impl
    implicit val ValueTraverse: Traverse[Value] = new Traverse[Value] {
      override def traverseImpl[G[_]: Applicative, A, B](fa: Value[A])(f: A => G[B]): G[Value[B]] =
        Applicative[G].pure(new Value[B] {})
    }
  }

  def main(args: Array[String]): Unit = {
    require(Functor[List] != null)

    case class Ping(i: Int)

    Message[Unit](
      List(
        RequiredField[Unit](
          Scalar[Unit](IntValue(42))
        )
      )
    )
  }

  def hash(x: GenMsg): Int = x.hylo(toHash, fromGenMsg)

//  def equals(x: GenMsg, y: GenMsg): Boolean = x.hylo(toEquals, fromGenMsg)

  def read[M[_]: Monad](s: In): M[GenMsg] =
    s.hyloM[M, Value, GenMsg](toGenMsgM, fromStream)

  def write[M[_]: Monad](x: GenMsg, s: Out): M[Unit] =
    x.hyloM[M, Value, Unit](toStream(s), fromGenMsgM)

  def size(x: GenMsg): Int = x.hylo(toSize, fromGenMsg)

  val fromGenMsg: Coalgebra[Value, GenMsg] = ???

  def fromGenMsgM[M[_]: Monad]: CoalgebraM[M, Value, GenMsg] = ???

  def fromStream[M[_]: Monad]: CoalgebraM[M, Value, In] = ??? //

  val toHash: Algebra[Value, Int] = {
    case Message(fields) => 42
    case Scalar(value)   => value.hash()
  }

  val toEquals: Algebra[Value, Boolean] = ???

  val toSize: Algebra[Value, Size] = ??? //

  //TODO find/extract a lift for `toGenMsg andThen Monad[M].pure`
  def toGenMsgM[M[_]: Monad]: AlgebraM[M, Value, GenMsg] = ???

  val toGenMsg: Algebra[Value, GenMsg] = ???

  //TODO use a writer monad?
  def toStream[M[_]: Monad](s: Out): AlgebraM[M, Value, Unit] = ???

  //generic utils:
  def pairWithInput[F[_]: Functor, A](alg: Algebra[F, A]): F[A] => F[(A, F[A])] =
    (fa: F[A]) => Functor[F].map(fa)(a => (alg(fa), fa))

}
