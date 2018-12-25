package scaloi.syntax

import org.scalatest._

import scala.reflect.ClassTag

class ClassTagOpsTest extends FlatSpec with OptionValues with Matchers {
  import classTag._

  behavior of "ClassTagOps"

  it should "cast safely" in {
    val ct: ClassTag[String] = ClassTag(classOf[String])
    ct.option(new java.lang.Integer(0)) should equal(None)
    ct.option("Bar") should equal(Some("Bar"))
    ct.unapply("Bar") should equal(Some("Bar"))
  }

  it should "typesafely extract the runtime class" in {
    val ctc: Class[String] = classTagClass[String]
    ctc should equal(classOf[String]) // actually it's compilation of the previous line we care about
  }

}
