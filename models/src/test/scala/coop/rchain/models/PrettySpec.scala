package coop.rchain.models

import coop.rchain.models.Connective.ConnectiveInstance.{Empty => _}
import coop.rchain.models.testImplicits._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


trait HasPretty {
  type tpe
  def pretty: String
}

case class MkPretty[A : Pretty](value: A) extends HasPretty {
  type tpe = A
  def pretty: String = s"MkPretty(${Pretty.pretty(value)})"
}

class PrettySpec extends FlatSpec with PropertyChecks with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(sizeRange = 50, minSuccessful = 10)

  behavior of "Pretty"

  consistentWithEquals

  implicit def prettyTuple10[
    T0: Pretty,
    T1: Pretty,
    T2: Pretty,
    T3: Pretty,
    T4: Pretty,
    T5: Pretty,
    T6: Pretty,
    T7: Pretty,
    T8: Pretty,
    T9: Pretty
  ] : Pretty[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)] = new Pretty[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)] {
    override def pretty(value: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9),
                        indentLevel: Int): String = {
        PrettyUtils.parenthesisedStrings(
          Seq(
            Pretty[T0].pretty(value._1, indentLevel + 1),
            Pretty[T1].pretty(value._2, indentLevel + 1),
            Pretty[T2].pretty(value._3, indentLevel + 1),
            Pretty[T3].pretty(value._4, indentLevel + 1),
            Pretty[T4].pretty(value._5, indentLevel + 1),
            Pretty[T5].pretty(value._6, indentLevel + 1),
            Pretty[T6].pretty(value._7, indentLevel + 1),
            Pretty[T7].pretty(value._8, indentLevel + 1),
            Pretty[T8].pretty(value._9, indentLevel + 1),
            Pretty[T9].pretty(value._10, indentLevel + 1),
          ),
          indentLevel
        )
    }}


  def consistentWithEquals: Unit =
    it must s"print compiling code evaluating to the original value" in {
      type T = (Par, Expr, Send, Receive, New, Match, ESet, EMap, ParSet, ParMap)
      val tPretty = prettyTuple10[Par, Expr, Send, Receive, New, Match, ESet, EMap, ParSet, ParMap]

      forAll { x: T =>
        val pretty = Pretty.pretty(x)(tPretty)
        println()
        println(pretty)
        println()
        println(pretty.count(_ == '\n'))
        val evaluated = evaluate[T](pretty, "(Par, Expr, Send, Receive, New, Match, ESet, EMap, ParSet, ParMap)")
        assert(x == evaluated)
        val evaluatedPretty = Pretty.pretty(evaluated)(tPretty)
        assert(evaluatedPretty == pretty)
      }
    }

  def evaluate[A](pretty: String, typeString: String): A = {
    val string =
      s"""
         |import coop.rchain.models._
         |import coop.rchain.models.Connective.ConnectiveInstance._
         |import coop.rchain.models.Expr.ExprInstance._
         |import coop.rchain.models.Var.VarInstance._
         |import coop.rchain.models.Var.WildcardMsg
         |import com.google.protobuf.ByteString
         |import scala.collection.immutable.BitSet
         |import monix.eval.Coeval
         |
         |val prettyCompiled: $typeString = $pretty
         |
         |prettyCompiled
      """.stripMargin


    import scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    val toolbox = currentMirror.mkToolBox()
    val tree    = toolbox.parse(string)
    toolbox.eval(tree).asInstanceOf[A]
  }
}
