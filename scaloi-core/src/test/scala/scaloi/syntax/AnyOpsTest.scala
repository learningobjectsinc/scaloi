package scaloi.syntax

import org.scalatest._
import scalaz.std.string._

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

  it should "provide a nifty monoidal inline null check" in {
    def foo(x: String): String = x ?| s"some$x"

    foo("thing") should equal("something")
    foo(null) should be("")

    "(1: Int) |? \"hi\"" shouldNot compile // Int not a reference type
    "\"bye\" |? List(1)" shouldNot compile // List[Int] not (known here to be) a monoid
  }

  it should "optionally transform" in {
    def startsWith(s: String)(t: String) = t.startsWith(s)
    ("foo" ?~> startsWith("f"))(_.toUpperCase) should equal("FOO")
    ("foo" ?~> startsWith("g"))(_.toUpperCase) should equal("foo")
  }

}