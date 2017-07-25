package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class FunctorOpsTest extends FlatSpec with OptionValues with Matchers {
  import FunctorOps._

  behavior of "FunctorOps"

  it should "partially map options" in {
    import scalaz.std.option._

    Option.empty[String] pfmap { case "A" => "B" } should equal(None)
    Option("A") pfmap { case "A"          => "B" } should equal(Some("B"))
    Option("C") pfmap { case "A"          => "B" } should equal(Some("C"))
  }

  it should "partially map lists" in {
    import scalaz.std.list._

    List("A", "C") pfmap { case "A" => "B" } should equal(List("B", "C"))
  }

}
