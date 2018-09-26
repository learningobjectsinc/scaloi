package scaloi
package syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.NonEmptyList
import scalaz.syntax.either._
import scaloi.test.ScaloiTest

import scala.util.{Failure, Success}

class OptionOpsTest
  extends FlatSpec
     with Matchers
     with OptionValues
     with ScaloiTest
{
  import OptionOps._

  behaviour of "OptionOps"

  it should "tap options" in {
    var state = 0
    Some(1) <|? { s =>
      state = s
    } should equal(Some(1))
    state should equal(1)
    None <|? { s: Int =>
      state = 2
    } should equal(None)
    state should equal(1)
    Some(3).tap(state = _) should equal(Some(3))
    state should equal(3)
  }

  it should "flat opt options" in {
    case class Something(s: String)
    Option(Something("a")).flatOpt(_.s) should equal(Some("a"))
    Option(Something(null)).flatOpt(_.s) should equal(None)
    Option(null.asInstanceOf[Something]).flatOpt(_.s) should equal(None)
  }

  it should "futurize options" in {
    val ex = new Exception

    Some(1).toFuture(ex).value.value should equal(Success(1))
    None.toFuture(ex).value.value should equal(Failure(ex))
  }

  it should "subtract options" in {
    import scalaz.std.anyVal._
    Some(1) - 1 should equal(None)
    Some(1) - 2 should equal(Some(1))
    None - 3 should equal(None)
  }

  it should "optionally transform" in {
    Some((_: Int) * 2) transforming 5 should equal(10)
    None transforming "Bolivia" should equal("Bolivia")

    Some((s: String) => s"${s}inator") ~?> "option" should equal("optioninator")
    None ~?> "King Wiggle-Wut the Greater" should equal(
      "King Wiggle-Wut the Greater")
  }

  it should "tap empty options" in {
    var state = 0
    Some(1) -<| { state = 1 } should equal(Some(1))
    state should be(0)
    None -<| { state = 2 } should equal(None)
    state should be(2)
  }

  it should "tryify options" in {
    case object UnfortunateHappenstance extends Error
    Some(1) toTry UnfortunateHappenstance should equal (Success(1))
    None toTry UnfortunateHappenstance should equal (Failure(UnfortunateHappenstance))
    Some(1) <@~* UnfortunateHappenstance should equal (Success(1))
    None <@~* UnfortunateHappenstance should equal (Failure(UnfortunateHappenstance))
    Some(1) elseFailure UnfortunateHappenstance should equal (Success(1))
  }

  it should "taskify options" in {
    import scalaz.syntax.either._
    case object UnfortunateHappenstance extends Error
    (Some(1) toTask UnfortunateHappenstance).unsafePerformSyncAttempt shouldEqual 1.right
    (None toTask UnfortunateHappenstance).unsafePerformSyncAttempt shouldEqual UnfortunateHappenstance.left
    (Some(1) *#@% UnfortunateHappenstance).unsafePerformSyncAttempt shouldEqual 1.right
    (None *#@% UnfortunateHappenstance).unsafePerformSyncAttempt shouldEqual UnfortunateHappenstance.left
  }

  it should "flatten-and-tryify try-wrapping options" in {
    final case class BadNumber(i: Int) extends Error
    final case class SadNumber(i: Int) extends Error
    Some(Success(1))            flatToTry BadNumber(2) should equal (Success(1))
    Some(Failure(SadNumber(1))) flatToTry BadNumber(2) should equal (Failure(SadNumber(1)))
    None                        flatToTry BadNumber(2) should equal (Failure(BadNumber(2)))
  }

  it should "flat left disjunct options" in {
    Some(1) <\/- 2.right should equal(1.left)
    Some(1) <\/- 2.left should equal(1.left)
    None <\/- 3.right should equal(3.right)
    None <\/- 4.left should equal(4.left)
  }

  it should "filter empties" in {
    import scalaz.std.string._

    OptionNZ("") should equal(None)
    OptionNZ("a") should equal(Some("a"))
    "OptionNZ(0)" shouldNot compile // no int zero in scope

    OptionNZ("A").orNZ("B") should equal(Some("A"))
    OptionNZ("").orNZ("B") should equal(Some("B"))
    OptionNZ("").orNZ("") should equal(None)

    Option("A").filterNZ should equal(Some("A"))
    Option("").filterNZ should equal(None)
  }

  it should "or z zeroes" in {
    import scalaz.std.list._
    Option(List(1)).orZ shouldEqual List(1)
    Option.empty[List[Int]].orZ shouldEqual Nil
  }

  it should "map new zealand style" in {
    import scalaz.std.anyVal._

    Option("0").nzMap(_.toInt) shouldEqual None
    Option("1").nzMap(_.toInt) shouldEqual Some(1)
    Option.empty[String].nzMap(_.toInt) shouldEqual None
  }

  it should "unboxtion" in {
    implicit class Baffle[T](t: T) {
      def baffle: T = t
    }

    (Boxtion(Boolean box true).baffle : Option[Boolean]) should be (Some(true))
    (Boxtion(Long box 3).baffle : Option[Long]) should be (Some(3L))

    val nothing: java.lang.Integer = null
    (Boxtion(nothing).baffle : Option[Int]) should be (None)
  }

  it should "max things" in {
    import scalaz.std.anyVal.intInstance

    Option.empty[Int].max(None) should equal(None)
    None.max(Some(1)) should equal(Some(1))
    Some(2).max(None) should equal(Some(2))
    Some(2).max(Some(3)) should equal(Some(3))
    Some(4).max(Some(3)) should equal(Some(4))
    None.max(1) should equal(1)
    Some(1).max(2) should equal(2)
    Some(3).max(2) should equal(3)
  }

  it should "min things" in {
    import scalaz.std.anyVal.intInstance

    Option.empty[Int].min(None) should equal(None)
    None.min(Some(1)) should equal(Some(1))
    Some(2).min(None) should equal(Some(2))
    Some(2).min(Some(3)) should equal(Some(2))
    Some(4).min(Some(3)) should equal(Some(3))
    None.min(1) should equal(1)
    Some(1).min(2) should equal(1)
    Some(3).min(2) should equal(2)
  }

  it should "make get or creates" in {

    Some("gotten") orCreate { "created" } should equal(Gotten("gotten"))
    Option.empty[Int] orCreate { 12 } should equal(Created(12))

    var state = false
    Some('bip) orCreate { state = true; 'dip } should equal(Gotten('bip))
    state should be(false)
  }

  it should "turn contained errors into failures" in {
    case class Mistake(badness: Int)
    case class MistakeException(mistake: Mistake) extends Throwable(mistake.toString)

    Option(Mistake(1)).thenFailure(MistakeException, 2) should matchPattern {
      case Failure(MistakeException(Mistake(1))) =>
    }

    None.thenFailure(MistakeException, 2) should matchPattern {
      case Success(2) =>
    }

    Option(Mistake(1)).thenHollowFailure(MistakeException) should matchPattern {
      case Failure(MistakeException(Mistake(1))) =>
    }

    None.thenHollowFailure(MistakeException) should matchPattern {
      case Success(()) =>
    }

    case class Woops() extends Throwable("uh-oh")

    Option(Woops()).toFailure(()) should matchPattern {
      case Failure(Woops()) =>
    }
    Option.empty[Woops].toFailure(()) should matchPattern {
      case Success(()) =>
    }

    Option(Woops()).thenInvalid(42)  should matchPattern {
      case scalaz.Failure(Woops()) =>
    }

    Option.empty[Woops].thenInvalid(42) should matchPattern {
      case scalaz.Success(42) =>
    }

    import scalaz.NonEmptyList

    Option(MistakeException(Mistake(11))).thenInvalidNel(42) should matchPattern {
      case scalaz.Failure(NonEmptyList((MistakeException(Mistake(11)), _))) =>
    }

    Option.empty[MistakeException].thenInvalidNel(42) should matchPattern {
      case scalaz.Success(42) =>
    }
  }

  it should "filter options independent of value" in {
    Some(1).when(true) shouldEqual Some(1)
    Some(1).when(false) shouldEqual None
    None.when(true) shouldEqual None

    Some(1).unless(true) shouldEqual None
    Some(1).unless(false) shouldEqual Some(1)
    None.unless(true) shouldEqual None
  }

  it should "map or zero" in {
    import scalaz.std.string._
    Option.empty[Int].foldZ(_.toString) shouldEqual ""
    Some(1).foldZ(_.toString) shouldEqual "1"
  }

  it should "accept runtime types" in {
    None.accept[String] shouldEqual None
    Some("hello").accept[Throwable] shouldEqual None
    Some("hello").accept[String] shouldEqual Some("hello")
  }

  it should "flatly opt conds" in {
    val f = flondOpt(_: String) {
      case "one" => Some("a")
      case "two" => None
    }

    f("one")   should be (Some("a"))
    f("two")   should be (None)
    f("three") should be (None)
  }

  it should "disjunct" in {
    Some(1).disjunct(_ + 1, 3) shouldEqual 2.right
    Option.empty[Int].disjunct(_ + 1, 3) shouldEqual 3.left
  }

  it should "divine truth" in {
    Option.empty[Boolean].isTrue shouldEqual false
    Some(false).isTrue shouldEqual false
    Some(true).isTrue shouldEqual true

    import java.{lang => jl}
    Option.empty[jl.Boolean].isTrue shouldEqual false
    Some(jl.Boolean.FALSE).isTrue shouldEqual false
    Some(jl.Boolean.TRUE).isTrue shouldEqual true
  }

  it should "validify" in {
    val errStr = "Value nonexistent"
    Option.empty[Int].elseInvalid(errStr) shouldEqual scalaz.Failure(errStr)
    Some(1).elseInvalid(errStr) shouldEqual scalaz.Success(1)
    Option.empty[Int].elseInvalidNel(errStr) shouldEqual scalaz.Failure(NonEmptyList(errStr))
    Some(1).elseInvalidNel(errStr) shouldEqual scalaz.Success(1)

    def errMsgExists(a: Int): String = s"Value exists: $a"
    Option.empty[Int].thenInvalid(errMsgExists _) shouldEqual scalaz.Success(())
    Some(1).thenInvalid(a => errMsgExists(a)) shouldEqual scalaz.Failure(errMsgExists(1))
    Option.empty[Int].thenInvalid(errMsgExists _) shouldEqual scalaz.Success(())
    Some(1).thenInvalidNel(errMsgExists _) shouldEqual scalaz.Failure(NonEmptyList(errMsgExists(1)))
  }
}
