package org.scalacheck

import cats.{Eq, Monad}
import cats.data.{IndexedStateT, NonEmptyList, StateT}
import cats.free.Free
import cats.laws.discipline.{AlternativeTests, MonadTests}
import cats.mtl.laws.discipline.MonadStateTests
import cats.tests.CatsSuite
import org.scalacheck.Gen.P
import org.scalacheck.rng.Seed
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class SubSpec extends FlatSpec with Matchers with PropertyChecks {

  behavior of "StateGen"

  sealed trait Exp[A]
  case class Lit[A](v: Int) extends Exp[A]
  case class Add[A](l: A, r: A) extends Exp[A]
  case class Let[A](n: String, v: A, r: A) extends Exp[A]
  case class Ref[A](n: String) extends Exp[A]

  type Prog[A] = Free[Exp, A]

  def lit[A](v: Int): Prog[A] = Free(Lit(v))
  def add[A](l: Prog[A], r: Prog[A]): Prog[A] = Free(Add(l , r))
  def let[A](n: String, v: Prog[A], r: Prog[A]): Prog[A] = Free(Let(n, v, r))
  def ref[A](n: String) = Free(Ref(n))

  it should "work" in {
    println("foo")
    import GenInstances._
    println(implicitly[Monad[StateT[Gen, Int, ?]]])
  }



}

object EqInstances {
  def sampledCogenEq[A](trials: Int)(implicit ev: Arbitrary[A]): Eq[Cogen[A]] =
    new Eq[Cogen[A]] {
      def eqv(x: Cogen[A], y: Cogen[A]): Boolean = {
        val gen : Gen[A] = ev.arbitrary
        val params : Gen.Parameters = Gen.Parameters.default
        // Loop Function which checks that the seeds from perturbing
        // given cogens create equivalent seeds for x iterations
        // to consider them equal
        def loop(count: Int, retries: Int, seed: Seed): Boolean =
          if (retries <= 0) sys.error("Generator Function Failed")
          else if (count <= 0) true // If make it through count all equal these are equal
          else {
            val rx = gen.doApply(params, seed) // Get Value
            rx.retrieve.fold(
              loop(count, retries - 1, rx.seed) // Loop As Necessary
            ){ a =>
              val seed = Seed.random
              val sx = x.perturb(seed, a)
              val sy = y.perturb(seed, a)
              if (sx != sy) false // If they are not equivalent
              else loop(count - 1, retries, rx.seed) // Another trial
            }
          }
        // Initiate Loop
        loop(trials, trials, Seed.random)
      }
    }
  def sampledGenEq[A: Eq](trials: Int): Eq[Gen[A]] = Eq.instance[Gen[A]]{ case (x, y) =>
    val params = Gen.Parameters.default
    def loop(count: Int, seed: Seed): Boolean =
      if (count <= 0) true
      else {
        // Leave this so the inequality creates the eq
        val tx = Try(x.doApply(params, seed))
        val ty = Try(y.doApply(params, seed))
        (tx, ty) match {
          case (Failure(_), Failure(_)) =>
            // They both failed, good, keep going
            loop(count - 1, Seed.random)
          case (Success(rx), Success(ry)) =>
            if (rx.retrieve != ry.retrieve) false
            else loop(count - 1, seed.next)
          case _ =>
            false
        }
      }
    loop(trials, Seed.random)
  }

}

trait ScalaCheckSetup {
//
  implicit def genEq[A: Eq]: Eq[Gen[A]] =
    EqInstances.sampledGenEq(1000)

  implicit def cogenEq[A: Arbitrary]: Eq[Cogen[A]] =
    EqInstances.sampledCogenEq(1000)

  implicit lazy val arbitrarySeed: Arbitrary[Seed] =
    Arbitrary(Gen.choose(Long.MinValue, Long.MaxValue).map(n => Seed(n)))

  implicit lazy val cogenSeed: Cogen[Seed] =
    Cogen[Long].contramap(_.long._1)

//  implicit def arbitraryNonEmptyList[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
//    Arbitrary(
//      (Arbitrary.arbitrary[A], Arbitrary.arbitrary[List[A]]).mapN(NonEmptyList(_, _))
//    )

  // Better Arbitrary Gen
  implicit def arbitraryGen[A: Arbitrary]: Arbitrary[Gen[A]] = {
    val simple = Gen.const(Arbitrary.arbitrary[A])
    val complex = Arbitrary.arbitrary[Seed => Seed].map { f =>
      Gen.gen((params, seed) => Arbitrary.arbitrary[A].doApply(params, f(seed)))
    }
    Arbitrary(Gen.oneOf(simple, complex))
  }
  //
  //  implicit def arbitraryCogen[A: Cogen]: Arbitrary[Cogen[A]] =
  //    Arbitrary(Arbitrary.arbitrary[Seed => Seed].map { f =>
  //      Cogen((seed, a) => f(Cogen[A].perturb(seed, a)))
  //    })
}

class GenLaws extends CatsSuite with ScalaCheckSetup {
  import GenInstances._

  type SGen[A] = StateT[Gen, Int, A]

  implicit def arbFAStaetT[A: Arbitrary]: Arbitrary[SGen[A]] = {
    Arbitrary[SGen[A]](Arbitrary.arbitrary[A].flatMap(a =>
      Gen.oneOf[SGen[A]](
        StateT.get[Gen, Int].as(a),
        StateT.modify[Gen, Int](_ + 1).as(a),
        StateT.modify[Gen, Int](_ - 1).as(a),
        StateT.modify[Gen, Int](_ * -1).as(a)
      )
    )

    )
  }




  implicit def eqFA[A: Eq]: Eq[SGen[A]] = {
//    implicit def eqGenA: Eq[Gen[A]] = EqInstances.sampledGenEq(1000)
    Eq.by(_.run(0))
  }

  // Tests Alternative
//  checkAll("Gen", AlternativeTests[Gen].alternative[Int, Int, Int])
  // Tests Monad
  checkAll("Gen", MonadTests[Gen].monad[Int, Int, Int])
  checkAll("Monad StateT Gen", MonadTests[SGen].monad[Int, Int, Int])

  import cats.mtl.implicits._

  checkAll("MonadStaete", MonadStateTests[SGen, Int].monadState[Int])

  // Tests FunctorFilter
  //  checkAll("Gen.FunctorFilterLaws", FunctorFilterTests[Gen].functorFilter[Int, Int, Int])
  //
  //  // Tests Monoid for Inner Given Monoid
  //  checkAll("Gen[String]", MonoidTests[Gen[String]].monoid)
  //  // Tests Low Priority Semigroup
  //  checkAll("Gen[NonEmptyList[Int]]", SemigroupTests[Gen[NonEmptyList[Int]]].semigroup)
}


object GenInstances {
  implicit val genInstances : Monad[Gen] = new Monad[Gen] {
    // Members declared in cats.Applicative
    override def pure[A](x: A): Gen[A] =
      Gen.const(x)

    // Members declared in cats.FlatMap
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => Gen[Either[A,B]]): Gen[B] =
      GenShims.tailRecM(a)(f)
  }

}


object GenShims {

  type P = Gen.Parameters

  import Gen.{R, r, gen}

  def tailRecM[A, B](a0: A)(fn: A => Gen[Either[A, B]]): Gen[B] = {

    @tailrec
    def tailRecMR(a: A, seed: Seed, labs: Set[String])(fn: (A, Seed) => R[Either[A, B]]): R[B] = {
      val re = fn(a, seed)
      val nextLabs = labs | re.labels
      re.retrieve match {
        case None => r(None, re.seed).copy(l = nextLabs)
        case Some(Right(b)) => r(Some(b), re.seed).copy(l = nextLabs)
        case Some(Left(a)) => tailRecMR(a, re.seed, nextLabs)(fn)
      }
    }

    // This is the "Reader-style" appoach to making a stack-safe loop:
    // we put one outer closure around an explicitly tailrec loop
    gen[B] { (p: P, seed: Seed) =>
      tailRecMR(a0, seed, Set.empty) { (a, seed) => fn(a).doApply(p, seed) }
    }
  }
}









