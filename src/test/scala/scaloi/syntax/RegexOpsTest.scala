package scaloi
package syntax

import org.scalatest.{FlatSpec, Matchers}
import scaloi.test.ScaloiTest

class RegexOpsTest extends FlatSpec with Matchers with ScaloiTest {
  import scaloi.syntax.regex._

  behaviour of "RegexOps"

  it should "fully match" in {
    "the world".r matches "the world" shouldBe true
    "the world".r matches "mothe worlds" shouldBe false
  }

  it should "partially match" in {
    "the world".r test "the world" shouldBe true
    "the world".r test "bothe worlds" shouldBe true
    "the word".r test "bothe worlds" shouldBe false
  }

  it should "prefix match" in {
    "the world".r lookingAt "the world is at an end" shouldBe true
    "the world".r lookingAt "bothe worlds are at an ond." shouldBe false
  }
}
