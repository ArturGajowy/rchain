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


  import org.scalacheck.Arbitrary._

  implicit def gen[T]: ArbF[F, T] = macro Magnolia.gen[T]

}


trait GenericArbLowPriority[F[_[_], _]] {

  implicit def monad: Monad[F[Gen, ?]]
  def liftF[A](gen: Gen[A]): F[Gen, A]

  type Typeclass[T] = ArbF[F, T]

  def combine[T](ctx: CaseClass[Typeclass, T]): ArbF[F, T] = {
    val paramFs: List[F[Gen, Any]] = ctx.parameters.toList.map(_.typeclass.arb.asInstanceOf[F[Gen, Any]])
    val fParams: F[Gen, List[Any]] = paramFs.sequence
    ArbF { fParams.map(ctx.rawConstruct) }
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): ArbF[F, T] = {
    val gens: List[F[Gen, T]] = ctx.subtypes.toList.map(_.typeclass.arb.asInstanceOf[F[Gen, T]])
    val chooseGen = liftF(Gen.oneOf(gens))
    ArbF[F, T] { monad.flatten(chooseGen) }
  }

}