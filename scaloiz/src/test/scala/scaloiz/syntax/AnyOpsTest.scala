package scaloiz.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scalaz.std.string._

class AnyOpsTest extends FlatSpec with OptionValues with Matchers {
  import AnyOps._

  behavior of "AnyOps"

  it should "provide a nifty monoidal inline null check" in {
    def foo(x: String): String = x ?| s"some$x"

    foo("thing") should equal("something")
    foo(null) should be("")

    "(1: Int) |? \"hi\"" shouldNot compile // Int not a reference type
    "\"bye\" |? List(1)" shouldNot compile // List[Int] not (known here to be) a monoid
  }

  it should "optionally transform" in {
    "foo".transformWhen(_.startsWith("f"))(_.toUpperCase) should equal("FOO")
    "foo".transformWhen(_.startsWith("g"))(_.toUpperCase) should equal("foo")
    "foo".transformUnless(_.startsWith("g"))(_.toUpperCase) should equal("FOO")
    "foo".transformUnless(_.startsWith("f"))(_.toUpperCase) should equal("foo")
    "A".transformNZ("B".concat) should equal("BA")
    "".transformNZ("B".concat) should equal("")
  }

}
