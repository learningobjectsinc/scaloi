package scaloi.misc

import java.{util => ju}

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class JavaBuildersTest extends FlatSpec with OptionValues with Matchers {
  import JavaBuilders._

  behavior of "JavaBuilders"

  it should "build lists" in {
    List("a", "b").to[ju.List] shouldBe a [ju.List[_]]
    List("a", "b").to[ju.List] should have size 2
    List("a", "b").to[ju.List].get(0) should be ("a")
  }

  it should "build sets" in {
    List("a", "b", "a").to[ju.Set] shouldBe a [ju.Set[_]]
    List("a", "b", "a").to[ju.Set] should have size 2
    List("a", "b", "a").to[ju.Set] should contain allOf ("a", "b")
  }

  ignore should "build maps" in {
    // scala/scala#5538 means this doesn't work...
    //Map("a" -> 1, "b" -> 2).to[ju.Map] shouldBe a [ju.Map[_,_]]

    // TODO: find a use case for this!
  }

}
