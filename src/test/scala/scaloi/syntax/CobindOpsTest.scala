package scaloi.syntax

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CobindOpsTest extends AnyFlatSpec with OptionValues with Matchers {
  import cobind._

  behavior of "CobindOps"

  it should "flat foreach copacetically" in {
    import scalaz.std.option._

    var value = Option("A")
    None coflatForeach { v: Option[String] => value = v }
    value should equal(Option("A"))
    Option("B") coflatForeach { v: Option[String] => value = v }
    value should equal(Option("B"))
  }
}
