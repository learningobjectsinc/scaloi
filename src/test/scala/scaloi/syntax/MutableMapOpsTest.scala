package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.std.anyVal._

import scala.collection.mutable

class MutableMapOpsTest extends FlatSpec with OptionValues with Matchers {
  import mutableMap._

  behavior of "MutableMapOps"

  it should "default to zero" in {
    val map = mutable.Map.empty[String, Int].withDefaultZero
    map.update("yes", 1)
    map("no") should equal(0)
    map.get("no") should equal(None)
    map("yes") should equal(1)
  }

  it should "update to zero" in {
    val map = mutable.Map.empty[String, Int]
    map.get("yes") should equal(None)
    map.getOrElseUpdateZ("yes") should equal(0)
    map.get("yes") should equal(Some(0))
  }

  it should "append semigroups" in {
    val map = mutable.Map.empty[String, Int].withDefaultZero
    map.update("yes", 1)
    map.append("no", 99)
    map.append("yes", 99)
    map("no") should equal(99)
    map("yes") should equal(100)
  }

}
