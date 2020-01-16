package scaloi.syntax

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag

class ClassTagOpsTest extends AnyFlatSpec with OptionValues with Matchers {
  import classTag._

  behavior of "ClassTagOps"

  it should "cast safely" in {
    val ct: ClassTag[String] = ClassTag(classOf[String])
    ct.option(java.lang.Integer.valueOf(0)) should equal(None)
    ct.option("Bar") should equal(Some("Bar"))
    ct.unapply("Bar") should equal(Some("Bar"))
  }

  it should "typesafely extract the runtime class" in {
    val ctc: Class[String] = classTagClass[String]
    ctc should equal(classOf[String]) // actually it's compilation of the previous line we care about
  }

}
