package scaloi.syntax

import javax.annotation.{Resource, Resources}

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClassOpsTest extends AnyFlatSpec with OptionValues with Matchers {

  import `class`._
  import ClassOpsTest._

  behavior of "ClassOps"

  val _defined = Symbol("defined")
  val _empty = Symbol("empty")

  it should "return class annotations" in {
    classOf[TestAnnotated].annotation[Resource] should be(_defined)
    classOf[TestAnnotated].annotation[Resources] should be(_empty)
  }

  it should "test class annotations" in {
    classOf[TestAnnotated].annotated[Resource] shouldBe true
    classOf[TestAnnotated].annotated[Resources] shouldBe false
  }

  it should "cast safely" in {
    classOf[String].option(new TestAnnotated) should equal(None)
    classOf[String].option("Bar") should equal(Some("Bar"))
  }

}

object ClassOpsTest {
  @Resource
  class TestAnnotated

}
