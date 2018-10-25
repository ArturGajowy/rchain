package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.models.BitSetBytesMapper._
import coop.rchain.models.Connective.ConnectiveInstance.{Empty => _}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.serialization.implicits._
import coop.rchain.models.testImplicits._
import coop.rchain.rspace.Serialize
import monix.eval.Coeval
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror


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
    PropertyCheckConfiguration(sizeRange = 1, minSuccessful = 1)

  behavior of "Pretty"

  consistentWithEquals


  def consistentWithEquals: Unit =
    it must s"print compiling code evaluating to the original value" in {
      val values: Gen[List[HasPretty]] = Gen.listOfN(10, Gen.oneOf(
        Arbitrary.arbitrary[Par].map(MkPretty(_)),
        Arbitrary.arbitrary[Expr].map(MkPretty(_)),
        Arbitrary.arbitrary[Send].map(MkPretty(_)),
        Arbitrary.arbitrary[Receive].map(MkPretty(_)),
        Arbitrary.arbitrary[New].map(MkPretty(_)),
        Arbitrary.arbitrary[Match].map(MkPretty(_)),
        Arbitrary.arbitrary[ESet].map(MkPretty(_)),
        Arbitrary.arbitrary[EMap].map(MkPretty(_)),
        Arbitrary.arbitrary[ParSet].map(MkPretty(_)),
        Arbitrary.arbitrary[ParMap].map(MkPretty(_)),
      ))

      forAll(values) { x: Seq[HasPretty] =>
        val pretty  = x.map(_.pretty).mkString("Seq(\n  ", ",\n  ", ")\n")
        println()
        println(pretty)
        println()
        println(pretty.count(_ == '\n'))
        val evaluated = evaluateSeq[HasPretty](pretty)
        assert(x == evaluated)
        val evaluatedPretty = evaluated.map(_.pretty).mkString("Seq(\n  ", ",\n  ", ")\n")
        assert(evaluatedPretty == pretty)
      }
    }

  def evaluateSeq[A](pretty: String)(
    implicit tag: ClassTag[A]
  ): Seq[A] = {
    val className = tag.runtimeClass.getCanonicalName
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
         |val prettyCompiled: Seq[$className] = $pretty
         |
         |prettyCompiled
      """.stripMargin


    import scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    val toolbox = currentMirror.mkToolBox()
    val tree    = toolbox.parse(string)
    toolbox.eval(tree).asInstanceOf[Seq[A]]
  }
}
