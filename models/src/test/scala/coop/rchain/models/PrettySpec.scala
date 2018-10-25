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

class PrettySpec extends FlatSpec with PropertyChecks with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(sizeRange = 20, minSuccessful = 5)

  behavior of "Pretty"

  consistentWithEquals[Par]
  consistentWithEquals[Expr]
  consistentWithEquals[Send]
  consistentWithEquals[Receive]
  consistentWithEquals[New]

  consistentWithEquals[Match]
  consistentWithEquals[ESet]
  consistentWithEquals[EMap]
  consistentWithEquals[ParSet]
  consistentWithEquals[ParMap]


  def consistentWithEquals[A: Arbitrary: Pretty](
      implicit tag: ClassTag[A]
  ): Unit =
    it must s"stably print compiling code evaluating to the original value for ${tag.runtimeClass.getSimpleName}" in {
      forAll(Gen.listOfN(10, Arbitrary.arbitrary[A])) { x: Seq[A] =>
          val pretty  = Pretty.pretty(x)
          val evaluated = evaluateSeq[A](pretty)
          assert(x == evaluated)
          assert(Pretty.pretty(evaluated) == pretty)
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
