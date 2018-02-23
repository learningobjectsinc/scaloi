package scaloi.misc

import java.util.concurrent.TimeUnit

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class JEnumEnumTest extends FlatSpec with OptionValues with Matchers {
  import JEnumEnum._

  behavior of "JEnumEnum"

  it should "znum jnums" in {
    import scalaz.syntax.enum._

    TimeUnit.NANOSECONDS.succ should equal(TimeUnit.MICROSECONDS)
    TimeUnit.NANOSECONDS.predx should equal(None)
  }
}