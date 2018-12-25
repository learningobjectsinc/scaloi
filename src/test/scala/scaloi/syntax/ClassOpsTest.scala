package scaloi.syntax

import javax.annotation.{Resource, Resources}

import org.scalatest._

class ClassOpsTest extends FlatSpec with OptionValues with Matchers {

  import `class`._
  import ClassOpsTest._

  behavior of "ClassOps"

  it should "return class annotations" in {
    classOf[TestAnnotated].annotation[Resource] should be('defined)
    classOf[TestAnnotated].annotation[Resources] should be('empty)
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
