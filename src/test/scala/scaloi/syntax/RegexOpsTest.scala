package scaloi
package syntax

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scaloi.test.ScaloiTest

class RegexOpsTest extends AnyFlatSpec with Matchers with ScaloiTest {
  import scaloi.syntax.regex._

  behaviour of "RegexOps"

  it should "partially match" in {
    "the world".r test "the world" shouldBe true
    "the world".r test "bothe worlds" shouldBe true
    "the word".r test "bothe worlds" shouldBe false
  }

  it should "prefix match" in {
    "the world".r lookingAt "the world is at an end" shouldBe true
    "the world".r lookingAt "bothe worlds are at an ond." shouldBe false
  }

  it should "match one" in {
    "abc(.*)".r match1 "abcdef" shouldBe Some("def")
    "abc(.*)".r match1 "abdcef" shouldBe None
  }
}
