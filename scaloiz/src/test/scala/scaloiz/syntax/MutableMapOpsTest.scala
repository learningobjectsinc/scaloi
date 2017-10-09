package scaloiz.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.collection.mutable
import scalaz.std.anyVal._

class MutableMapOpsTest extends FlatSpec with OptionValues with Matchers {
  import MutableMapOps._

  behavior of "MutableMapOps"

  it should "default to zero" in {
    val map = mutable.Map.empty[String, Int].withDefaultZero
    map.update("yes", 1)
    map("no") should equal(0)
    map("yes") should equal(1)
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
