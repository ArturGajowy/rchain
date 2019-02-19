package org.scalacheck

import cats.Monad
import magnolia._

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
    val paramFs: List[F[Gen, Any]] = ctx.parameters.toList.map(_.typeclass.arb.asInstanceOf[F[Gen, Any]])
    val fParams: F[Gen, List[Any]] = paramFs.sequence
    ArbF { for {
      done <- fParams.map(ctx.rawConstruct)
      l <- liftF(Gen.lzy(done))
    } yield l }
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): ArbF[F, T] = {
    def gens: List[F[Gen, T]] = ctx.subtypes.toList.map(_.typeclass.arb.asInstanceOf[F[Gen, T]])
    def chooseGen = liftF(Gen.lzy(Gen.oneOf(gens)))
    ArbF[F, T] { monad.flatten(chooseGen) }
  }


  def gen[T]: ArbF[F, T] = macro Magnolia.gen[T]

}


trait GenericArbLowPriority[F[_[_], _]] {

  def liftF[A](gen: Gen[A]): F[Gen, A]
  import org.scalacheck.Arbitrary._

  implicit def liftArbitrary[A: Arbitrary]: ArbF[F, A] = ArbF[F, A](liftF[A](Arbitrary.arbitrary[A]))

}