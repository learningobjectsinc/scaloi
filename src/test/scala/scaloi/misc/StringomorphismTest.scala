package scaloi.misc

import java.time.Instant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class StringomorphismTest extends AnyFlatSpec with Matchers {
  behavior of "Stringomorphism"

  val _failure = Symbol("failure")

  it should "morph strings to strings" in {
    Stringomorphism[String].apply("super") shouldEqual Success("super")
  }

  it should "morph strings to instants" in {
    val instant = Instant.now
    Stringomorphism[Instant].apply("sad") should be(_failure)
    Stringomorphism[Instant].apply(instant.toString) shouldEqual Success(instant)
  }

  it should "morph strings to booleans" in {
    Stringomorphism[Boolean].apply("sad") should be(_failure)
    Stringomorphism[Boolean].apply("false") shouldEqual Success(false)
    Stringomorphism[Boolean].apply("true") shouldEqual Success(true)
  }

  it should "morph strings to longs" in {
    Stringomorphism[Long].apply("sad") should be(_failure)
    Stringomorphism[Long].apply("12345") shouldEqual Success(12345L)
  }

  it should "morph strings to enumerata" in {
    import StringomorphismTest._
    Stringomorphism[E1].apply("sad") should be(_failure)
    Stringomorphism[E1].apply("A") shouldEqual Success(E1.A)
  }
}

object StringomorphismTest {

  sealed trait E1 extends enumeratum.EnumEntry

  private object E1 extends enumeratum.Enum[E1] {
    val values = findValues

    case object A extends E1

    case object B extends E1

  }

}
