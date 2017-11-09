package scaloi.syntax

import org.scalatest._



class AnyOpsTest extends FlatSpec with OptionValues with Matchers {
  import AnyOps._

  behavior of "AnyOps"

  it should "kestrel combinate" in {
    var state = ""
    "a" <| { s =>
      state = s
    } should equal("a")
    state should equal("a")

    "b".tap(s => state = s) should equal("b")
    state should equal("b")
  }

  it should "partially kestrel combinate" in {
    var state = ""
    "a" pfTap {
      case s: String => state = s
    } should equal("a")
    state should equal("a")

    "b" pfTap {
      // a full tap would throw here
      case s: String if s == "d" => state = "b"
    } should equal("b")
    state should equal("a")
  }

  it should "thrush combinate" in {
    "a" |> { s =>
      s + "b"
    } should equal("ab")
  }



}
