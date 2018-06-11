package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class CobindOpsTest extends FlatSpec with OptionValues with Matchers {
  import CobindOps._

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
