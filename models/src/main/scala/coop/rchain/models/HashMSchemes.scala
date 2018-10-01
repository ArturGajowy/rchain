package coop.rchain.models

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import matryoshka._
import matryoshka.implicits._
import scalapb.GeneratedMessage
import scalaz.Scalaz._
import scalaz.{Applicative, Free, Functor, Monad, Traverse}

import scala.language.higherKinds

object HashMSchemes {

  type GenMsg = Toy
  sealed trait Toy
  case class Foo(v: Int, nested: Toy)
  case class Bar(v: String)

  sealed trait Value[A]
  case class Message[A](fields: List[Field[A]]) extends Value[A]
  case class Scalar[A](value: ScalarValue)      extends Value[A]

  sealed trait Field[A]
  case class RequiredField[A](value: A)            extends Field[A]
  case class OptionalField[A](valueOpt: Option[A]) extends Field[A]
  case class RepeatedField[A](values: List[A])     extends Field[A]

  trait ScalarValue {
    type T
    val value: T
    def hash(): Int = value.hashCode()
  }
  case class IntValue(value: Int) extends ScalarValue { type T = Int }

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

  //FIXME where's tha fix!!!!!!!!!!!!!!!

  def hash(x: GenMsg): Int = x.hylo(toHash, fromGenMsg)

  val fromGenMsg: Coalgebra[Value, GenMsg] = {
    case p: Product => ???
  }

  val toHash: Algebra[Value, Int] = {
    case Message(fields) => 42
    case Scalar(value)   => value.hash()
  }

  def main(args: Array[String]): Unit =
    hash(Par())
}
