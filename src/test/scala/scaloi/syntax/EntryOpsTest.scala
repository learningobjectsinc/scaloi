package scaloi.syntax

import java.util

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.collection.JavaConverters._

class EntryOpsTest extends FlatSpec with OptionValues with Matchers {
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
