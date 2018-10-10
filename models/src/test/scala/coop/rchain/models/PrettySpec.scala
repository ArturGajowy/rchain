package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.models.BitSetBytesMapper._
import coop.rchain.models.Connective.ConnectiveInstance.{Empty => _}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.serialization.implicits._
import coop.rchain.models.testImplicits._
import coop.rchain.rspace.Serialize
import monix.eval.Coeval
import org.scalacheck.{Arbitrary, Shrink}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

class PrettySpec extends FlatSpec with PropertyChecks with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(sizeRange = 250, minSuccessful = 1000)

  behavior of "Pretty"

  consistentWithEquals[Par]
  consistentWithEquals[Expr]
  consistentWithEquals[Var]
  consistentWithEquals[Send]
  consistentWithEquals[Receive]
  consistentWithEquals[New]
  consistentWithEquals[Match]
  consistentWithEquals[ESet]
  consistentWithEquals[EMap]

  it must "work" in {
    val x =
      (
        Expr(
          EMapBody(
            ParMap(
              SortedParMap(Map(
                Par(
                  ids = Seq(
                    GPrivate(),
                    GPrivate(
                      ByteString.copyFrom(Array[Byte](
                        -107
                      ))
                    )
                  ),
                  locallyFree = AlwaysEqual(BitSet()),
                  connectiveUsed = true
                ) -> Par(
                  locallyFree = AlwaysEqual(BitSet(0, 1, 2, 3, 5, 10, 11, 16, 17, 18, 19, 20, 25, 29, 30, 31, 34, 35, 36, 37, 38, 39, 41, 42, 43, 53, 54)),
                  connectiveUsed = true
                )
              )),
              true,
              Coeval.Now(BitSet(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 257, 258, 259, 262, 265, 266, 267, 268, 272, 273, 276, 277, 278, 280, 281, 283, 284, 285, 290, 292, 294, 295, 296, 297, 299, 303, 304, 305, 312, 313, 315, 317, 319, 383, 384, 385, 386, 388, 389, 390, 393, 395, 396, 397, 401, 404, 405, 406, 409, 410, 411, 412, 414, 415, 416, 418, 422, 423, 425, 427, 429, 430, 431, 433, 435, 438, 439, 440, 443, 445, 448, 512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529, 530, 531, 532, 533, 534, 535, 536, 537, 538, 539, 540, 541, 542, 543, 544, 545, 546, 547, 548, 549, 550, 551, 552, 553, 554, 555, 556, 557, 558, 559, 560, 561, 562, 563, 564, 565, 566, 567, 568, 569, 570, 571, 572, 573, 574, 576)),
              None
            )
          )
        )
      )

    val y =
      (
        Expr(
          EMapBody(
            ParMap(
              SortedParMap(Map(
                Par(
                  ids = Seq(
                    GPrivate(
                      ByteString.copyFrom(Array[Byte]())
                    ),
                    GPrivate(
                      ByteString.copyFrom(Array[Byte](
                        -107
                      ))
                    )
                  ),
                  locallyFree = AlwaysEqual(BitSet()),
                  connectiveUsed = true
                ) -> Par(
                  locallyFree = AlwaysEqual(BitSet(0, 1, 2, 3, 5, 10, 11, 16, 17, 18, 19, 20, 25, 29, 30, 31, 34, 35, 36, 37, 38, 39, 41, 42, 43, 53, 54)),
                  connectiveUsed = true
                )
              )),
              true,
              Coeval.now(BitSet(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 257, 258, 259, 262, 265, 266, 267, 268, 272, 273, 276, 277, 278, 280, 281, 283, 284, 285, 290, 292, 294, 295, 296, 297, 299, 303, 304, 305, 312, 313, 315, 317, 319, 383, 384, 385, 386, 388, 389, 390, 393, 395, 396, 397, 401, 404, 405, 406, 409, 410, 411, 412, 414, 415, 416, 418, 422, 423, 425, 427, 429, 430, 431, 433, 435, 438, 439, 440, 443, 445, 448, 512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529, 530, 531, 532, 533, 534, 535, 536, 537, 538, 539, 540, 541, 542, 543, 544, 545, 546, 547, 548, 549, 550, 551, 552, 553, 554, 555, 556, 557, 558, 559, 560, 561, 562, 563, 564, 565, 566, 567, 568, 569, 570, 571, 572, 573, 574, 576)),
              None
            )
          )
        )
      )

    equalPrettyMeansEqualValues(x, y)
  }

  def consistentWithEquals[A: Serialize: Arbitrary: Shrink: Pretty](
      implicit tag: ClassTag[A]): Unit =
    it must s"print different values as different strings for ${tag.runtimeClass.getSimpleName}" in {
      forAll { (x: A, y: A) =>
        equalPrettyMeansEqualValues(x, y)
      }
    }

  def equalPrettyMeansEqualValues[A: Serialize: Arbitrary: Shrink: Pretty](x: A,
                                                                           y: A): Assertion = {
    val px = Pretty.pretty(x)
    if (px == Pretty.pretty(y)) {
      println("Ping!")
      println(px)
      assert(x == y)
    } else {
      assert(x != y)
    }
  }
}
