package scaloiz.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class FunctorOpsTest extends FlatSpec with OptionValues with Matchers {
  import FunctorOps._

  behavior of "FunctorOps"

  it should "partially map options" in {
    import scalaz.std.option._

    Option.empty[String] pfMap { case "A" => "B" } should equal(None)
    Option("A") pfMap { case "A"          => "B" } should equal(Some("B"))
    Option("C") pfMap { case "A"          => "B" } should equal(Some("C"))
  }

  it should "partially map lists" in {
    import scalaz.std.list._

    List("A", "C") pfMap { case "A" => "B" } should equal(List("B", "C"))
  }

}
