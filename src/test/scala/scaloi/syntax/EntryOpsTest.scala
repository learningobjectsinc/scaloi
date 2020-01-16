package scaloi.syntax

import java.util

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.JavaConverters._

class EntryOpsTest extends AnyFlatSpec with OptionValues with Matchers {
  import entry._

  behavior of "EntryOps"

  it should "scalatize jentries" in {

    val a = new util.HashMap[String, String]()
    a.put("alpha", "beta")
    a.put("aleph", "beth")
    a.entrySet.asScala.map(_.asScala) should equal(
      Map("alpha" -> "beta", "aleph" -> "beth").toSet)
  }

}
