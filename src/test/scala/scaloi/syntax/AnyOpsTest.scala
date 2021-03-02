package scaloi.syntax

import org.scalatest.{OptionValues, TryValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scalaz.std.string._
import scalaz.syntax.either._

class AnyOpsTest extends AnyFlatSpec with OptionValues with TryValues with Matchers {
  import any._

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
    "foo".transformWhen(_.startsWith("f"))(_.toUpperCase) should equal("FOO")
    "foo".transformWhen(_.startsWith("g"))(_.toUpperCase) should equal("foo")
    "foo".transformUnless(_.startsWith("g"))(_.toUpperCase) should equal("FOO")
    "foo".transformUnless(_.startsWith("f"))(_.toUpperCase) should equal("foo")
    "A".transformNZ("B".concat) should equal("BA")
    "".transformNZ("B".concat) should equal("")
  }

  it should "predicately disjunct" in {
    "true".rightWhen(_.toBoolean)("left") should equal("true".right)
    "false".rightWhen(_.toBoolean)("left") should equal("left".left)
    "true".rightUnless(_.toBoolean)("left") should equal("left".left)
    "false".rightUnless(_.toBoolean)("left") should equal("false".right)
  }

  it should "functorly assoc" in {
    import scalaz.std.option._
    import scalaz.std.list._
    "a" -*> Option.empty shouldEqual None
    "a" -*> Option(2) shouldEqual Some("a" -> 2)
    "a" -*> List(1, 2) shouldEqual List("a" -> 1, "a" -> 2)
  }

  it should "cast safely" in {
    "a".asInstanceOf_![AnyOpsTest].failure.exception shouldBe a[ClassCastException]
    "a".asInstanceOf_![AnyRef].success.value shouldEqual "a"
  }

  it should "cast optionally" in {
    "a".asInstanceOf_?[AnyOpsTest] shouldEqual None
    "a".asInstanceOf_?[AnyRef] shouldEqual Some("a")
  }

  behavior of "sins"

  it should "convert any value to an Option" in {
    sins.any2Option(null) shouldBe None
    sins.any2Option("a").value shouldBe "a"
  }

}
