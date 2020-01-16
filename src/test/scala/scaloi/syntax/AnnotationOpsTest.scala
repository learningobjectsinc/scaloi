package scaloi.syntax

import javax.annotation.Resource
import javax.annotation.Resource.AuthenticationType

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnnotationOpsTest extends AnyFlatSpec with OptionValues with Matchers {
  import annotation._
  import AnnotationOpsTest._

  behavior of "AnnotationOps"

  it should "override annotation properties" in {
    val method = classOf[TestAnnotated].getDeclaredMethod("annotated")
    val annotation = method.getAnnotation(classOf[Resource])
    annotation should have('name ("fred"), 'authenticationType (AuthenticationType.CONTAINER), 'shareable (true))
    val attributed = annotation ++ Map("name" -> "jim", "authenticationType" -> AuthenticationType.APPLICATION)
    attributed should have('name ("jim"), 'authenticationType (AuthenticationType.APPLICATION), 'shareable (true))
  }
}

object AnnotationOpsTest {
  class TestAnnotated {
    @Resource(name = "fred")
    def annotated(): Unit = ()
  }
}
