package coop.rchain.rholang.interpreter.matcher

import cats._
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object EffMatcher {

  type StateCost[A]       = State[Cost, A]
  type EitherOutOfPhlo[A] = Either[OutOfPhlogistonsError.type, A]
  type StateFreeMap[A]    = State[FreeMap, A]

  type _costLimiting[R]     = EitherOutOfPhlo |= R
  type _costTracking[R]     = StateCost |= R
  type _singlyMatching[R]   = Option |= R
  type _multiplyMatching[R] = List |= R
  type _captureTracking[R]  = StateFreeMap |= R

  def charge[R: _costLimiting: _costTracking](amount: Cost): Eff[R, Unit] =
    for {
      currentCost <- get
      newCost     = currentCost - amount
      effect <- if (newCost.value < 0) left(OutOfPhlogistonsError)
               else put[R, Cost](newCost) >>= right[R, OutOfPhlogistonsError.type, Unit]
    } yield effect
//
//  def attemptOpt[R: _singlyMatching: _captureTracking]: Eff[R, Option]

}
