package scaloi.misc

import java.{util => ju}

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JavaBuildersTest extends AnyFlatSpec with OptionValues with Matchers {
  import JavaBuilders._

  behavior of "JavaBuilders"

  it should "build lists" in {
    List("a", "b").to(JavaList) shouldBe a [ju.List[_]]
    List("a", "b").to(JavaList) should have size 2
    List("a", "b").to(JavaList).get(0) should be ("a")
  }

  it should "build sets" in {
    List("a", "b", "a").to(JavaSet) shouldBe a [ju.Set[_]]
    List("a", "b", "a").to(JavaSet) should have size 2
    List("a", "b", "a").to(JavaSet) should contain.allOf ("a", "b")
  }

  ignore should "build maps" in {
    // scala/scala#5538 means this doesn't work...
    //Map("a" -> 1, "b" -> 2).to[ju.Map] shouldBe a [ju.Map[_,_]]

    // TODO: find a use case for this!
  }

}
