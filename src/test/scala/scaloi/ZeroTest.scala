package scaloi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scaloi.test.ScaloiTest

class ZeroTest extends AnyFlatSpec with Matchers with ScaloiTest {
  behaviour of "Zero"

  it should "zero numerics" in {
    import scaloi.Zero._
    import scaloi.syntax.zero._

    0.isZero shouldEqual true
    zero[Int] shouldEqual 0
    Zero[Int].isZero(0) shouldBe true
    Zero[Int].isZero(1) shouldBe false
  }

  it should "zero monoids" in {
    import scalaz.std.string._
    import scaloi.Zero._

    zero[String] shouldEqual ""
    Zero[String].isZero("") shouldBe true
    Zero[String].isZero("a") shouldBe false
  }

  it should "zero cbfs" in {
    import scaloi.std.collection._
    import scaloi.Zero._

    zero[Seq[Int]] shouldEqual Seq.empty
    Zero[Seq[Int]].isZero(Nil) shouldBe true
    Zero[Seq[Int]].isZero(Seq(1)) shouldBe false
  }

  it should "zero is-empties" in {
    import scalaz.std.list._
    import scaloi.Zero._

    zero[List[Int]] shouldEqual Nil
    Zero[List[Int]].isZero(Nil) shouldBe true
    Zero[List[Int]].isZero(List(1)) shouldBe false
  }

  it should "zero ju.List" in {
    import scaloi.std.ju._
    import scaloi.Zero._
    import java.util.{Collections, List => JList}

    zero[JList[Int]] shouldEqual Collections.emptyList[Int]
    Zero[JList[Int]].isZero(Collections.emptyList[Int]) shouldBe true
    Zero[JList[Int]].isZero(Collections.singletonList(1)) shouldBe false
  }

  it should "provide syntax" in {
    import scalaz.std.string._
    import scaloi.syntax.zero._

    0.isZero shouldBe true
    0.nonZero shouldBe false
    1.isZero shouldBe false
    1.nonZero shouldBe true

    "Hello".zNull shouldBe "Hello"
    (null: String).zNull shouldBe ""

    0 ||| 1 shouldEqual 1
  }
}
