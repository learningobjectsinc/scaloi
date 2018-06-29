package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.-\/
import scalaz.syntax.either._
import scalaz.syntax.std.boolean._
import scaloi.test.ScaloiTest

class BooleanOpsTest
  extends FlatSpec
     with Matchers
     with OptionValues
     with ScaloiTest
{
  import BooleanOps._

  behaviour of "BooleanOps"

  it should "support or else conditional eithers" in {
    true either "Happy" or "Sad" should equal("Happy".right)
    false either "Happy" or "Sad" should equal("Sad".left)
    true either "Happy" orElse "Sad".right should equal("Happy".right)
    false either "Happy" orElse "Sad".right should equal("Sad".right)
    false either "Happy" orElse -\/("Sad") should equal("Sad".left)
  }

  it should "flat option" in {

    true flatOption Some(1) should equal(Some(1))
    false flatOption Some(1) should equal(None)
    true flatOption None should equal(None)
    false flatOption None should equal(None)

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

  it should "enrich jooleans" in {
    import java.{lang => jl}
    jl.Boolean.TRUE.option(1) shouldEqual Some(1)
    jl.Boolean.FALSE.option(1) shouldEqual None
    jl.Boolean.TRUE.noption(1) shouldEqual None
    jl.Boolean.FALSE.noption(1) shouldEqual Some(1)
  }

}
