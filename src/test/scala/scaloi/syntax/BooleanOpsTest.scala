package scaloi.syntax

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaz.{-\/, NonEmptyList}
import scalaz.syntax.either._
import scalaz.syntax.std.boolean._
import scaloi.test.ScaloiTest

import scala.util.{Failure, Success}

class BooleanOpsTest
  extends AnyFlatSpec
     with Matchers
     with OptionValues
     with ScaloiTest
{
  import boolean._

  behaviour of "BooleanOps"

  it should "support or else conditional eithers" in {
    true either "Happy" or "Sad" should equal("Happy".right)
    false either "Happy" or "Sad" should equal("Sad".left)
    true either "Happy" orElse "Sad".right[Int] should equal("Happy".right)
    false either "Happy" orElse "Sad".right[Int] should equal("Sad".right)
    false either "Happy" orElse -\/("Sad") should equal("Sad".left)
  }

  it should "support or failure conditional eithers" in {
    object err extends Error
    true either "Happy" orFailure err shouldEqual Success("Happy")
    false either "Sad" orFailure err shouldEqual Failure(err)
  }

  it should "flat option" in {

    true flatOption Some(1) should equal(Some(1))
    false flatOption Some(1) should equal(None)
    true flatOption None should equal(None)
    false flatOption None should equal(None)

    false flatNoption Some(1) should equal(Some(1))
    true flatNoption Some(1) should equal(None)
    false flatNoption None should equal(None)
    true flatNoption None should equal(None)

    true ?-? Some("maybe") should equal (Some("maybe"))
    false ?-? Some("maybe") should equal (None)
    true ?-? None should equal (None)
    false ?-? None should equal (None)
  }

  it should "???" in {
    import scalaz.std.string._
    import scalaz.syntax.std.option._

    val none = Option.empty[String]

    true  ??? "foo".some should be ("foo")
    true  ???       none should be ("")
    false ??? "foo".some should be ("")
    false ???       none should be ("")
  }

  it should "conditionally run side effects" in {
    var state = "a"
    true <|? {
      state = "b"
    } should equal(true)
    state should equal("b")
    false <|? {
      state = "c"
    } should equal(false)
    state should equal("b")

    true <|! {
      state = "d"
    } should equal(true)
    state should equal("b")
    false <|! {
      state = "e"
    } should equal(false)
    state should equal("e")
  }

  it should "conditionally disjunct" in {
    true.thenLeft("hello") should equal("hello".left[Boolean])
    false.thenLeft("hello") should equal(false.right[String])
    false.elseLeft("hello") should equal("hello".left[Boolean])
    true.elseLeft("hello") should equal(true.right[String])
    false \/> "hello" should equal("hello".left[Boolean])
  }

  it should "noption" in {
    true.noption("A") should equal(None)
    false.noption("A") should equal(Some("A"))
  }

  it should "else failure" in {
    import scala.util._

    object err extends Error
    true.elseFailure(err) should equal (Success(true))
    false.elseFailure(err) should equal (Failure(err))

    true <@~* err should equal (Success(true))
    false <@~* err should equal (Failure(err))
  }

  it should "then failure" in {
    import scala.util._

    object err extends Error
    false.thenFailure(err) should equal (Success(false))
    true.thenFailure(err) should equal (Failure(err))

    false *~@> err should equal (Success(false))
    true *~@> err should equal (Failure(err))
  }

  it should "support validation" in {
    import scalaz.{Failure, Success}

    true.elseInvalid("Error", 42) shouldEqual Success(42)
    false.elseInvalid("Error", 42) shouldEqual Failure("Error")
    true.elseInvalidNel("Error", 42) shouldEqual Success(42)
    false.elseInvalidNel("Error", 42) shouldEqual Failure(NonEmptyList("Error"))

    false.thenInvalid("Error", 42) shouldEqual Success(42)
    true.thenInvalid("Error", 42) shouldEqual Failure("Error")
    false.thenInvalidNel("Error", 42) shouldEqual Success(42)
    true.thenInvalidNel("Error", 42) shouldEqual Failure(NonEmptyList("Error"))

    true elseInvalid "Error" shouldEqual Success(())
    false elseInvalid "Error" shouldEqual Failure("Error")
    true elseInvalidNel "Error" shouldEqual Success(())
    false elseInvalidNel "Error" shouldEqual Failure(NonEmptyList("Error"))

    false thenInvalid "Error" shouldEqual Success(())
    true thenInvalid "Error" shouldEqual Failure("Error")
    false thenInvalidNel "Error" shouldEqual Success(())
    true thenInvalidNel "Error" shouldEqual Failure(NonEmptyList("Error"))
  }

  it should "enrich jooleans" in {
    import java.{lang => jl}
    jl.Boolean.TRUE.option(1) shouldEqual Some(1)
    jl.Boolean.FALSE.option(1) shouldEqual None
    jl.Boolean.TRUE.noption(1) shouldEqual None
    jl.Boolean.FALSE.noption(1) shouldEqual Some(1)
  }

  it should "optionally estream" in {
    false optionES 0 shouldBe empty
    (true optionES 1).toList shouldEqual List(1)
  }
}
