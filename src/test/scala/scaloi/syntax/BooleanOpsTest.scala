package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.-\/
import scalaz.syntax.either._
import scalaz.syntax.std.`boolean`._

class BooleanOpsTest extends FlatSpec with OptionValues with Matchers {
  import BooleanOps._

  behavior of "BooleanOps"

  it should "support or else conditional eithers" in {
    // scalatest defines a conflicting String.left
    true either "Happy" or "Sad" should equal("Happy".right)
    false either "Happy" or "Sad" should equal(-\/("Sad"))
    true either "Happy" orElse "Sad".right should equal("Happy".right)
    false either "Happy" orElse "Sad".right should equal("Sad".right)
    false either "Happy" orElse -\/("Sad") should equal(-\/("Sad"))
  }

  it should "flat option" in {

    true flatOption Some(1) should equal(Some(1))
    false flatOption Some(1) should equal(None)
    true flatOption None should equal(None)
    false flatOption None should equal(None)
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
    true.thenLeft("hello") should equal("hello".left[Unit])
    false.thenLeft("hello") should equal(().right[String])
    false.elseLeft("hello") should equal("hello".left[Unit])
    true.elseLeft("hello") should equal(().right[String])
    false \/> "hello" should equal("hello".left[Unit])
  }

  it should "noption" in {
    true.noption("A") should equal(None)
    false.noption("A") should equal(Some("A"))
  }

  it should "else failure" in {
    import scala.util._

    object err extends Error
    true.elseFailure(err) should equal (Success(()))
    false.elseFailure(err) should equal (Failure(err))
  }

  it should "then failure" in {
    import scala.util._

    object err extends Error
    false.thenFailure(err) should equal (Success(()))
    true.thenFailure(err) should equal (Failure(err))
  }

  it should "enrich jooleans" in {
    import java.{lang => jl}
    jl.Boolean.TRUE.option(1) shouldEqual Some(1)
    jl.Boolean.FALSE.option(1) shouldEqual None
    jl.Boolean.TRUE.noption(1) shouldEqual None
    jl.Boolean.FALSE.noption(1) shouldEqual Some(1)
  }

}
