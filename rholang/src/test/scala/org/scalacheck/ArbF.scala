package org.scalacheck

import cats.data.{OptionT, StateT}
import cats.{Defer, Monad}
import magnolia._
import monix.eval.Coeval

import scala.language.experimental.macros

trait ArbF[F[_[_], _], T] { def arb: F[Gen, T] }

object ArbF {

  def arbF[F[_[_], _], T](implicit ev: ArbF[F, T]): F[Gen, T] = ev.arb

  def apply[F[_[_], _], T](g: F[Gen, T]): ArbF[F, T] =
    new ArbF[F, T] {
      override def arb: F[Gen, T] = g
    }
}

import cats.implicits._

trait GenericArb[F[_[_], _]] extends GenericArbLowPriority[F] {


  implicit def monad: Monad[F[Gen, ?]]

  type Typeclass[T] = ArbF[F, T]

  def combine[T](ctx: CaseClass[Typeclass, T]): ArbF[F, T] = {
    println(s"combine ${ctx.typeName.short}")

    ArbF(defer.defer {
      println(s"defer combine ${ctx.typeName.short}")
      val paramFs: List[F[Gen, Any]] =
        ctx.parameters.toList.map(_.typeclass.arb.asInstanceOf[F[Gen, Any]])
      val fParams: F[Gen, List[Any]] = paramFs.sequence
      fParams.map { a =>
        println(s"run combine ${ctx.typeName.short}");
        a
      }.map(ctx.rawConstruct)
    }.map { a => println("return combine "+ a); a})
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): ArbF[F, T] = {
    println(s"dispatch ${ctx.typeName.short}")
    ArbF(defer.defer {
      for {
        subtype <- liftF(Gen.oneOf(ctx.subtypes)).map { a => println(s"Chosen: ${a.typeName.short}"); a}
        gen <- subtype.typeclass.arb.asInstanceOf[F[Gen, T]]
      } yield gen
    }.map { a => println("return dispatch "+ a); a})
  }


  def gen[T]: ArbF[F, T] = macro Magnolia.gen[T]

}


trait GenericArbLowPriority[F[_[_], _]] {
  implicit def defer: Defer[F[Gen, ?]]

  def liftF[A](gen: Gen[A]): F[Gen, A]
  import org.scalacheck.Arbitrary._

  implicit def liftArbitrary[A: Arbitrary]: ArbF[F, A] = ArbF[F, A](defer.defer(liftF[A](Arbitrary.arbitrary[A])))

}