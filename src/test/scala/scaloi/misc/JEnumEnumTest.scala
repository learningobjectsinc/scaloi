package scaloi.misc

import java.util.concurrent.TimeUnit

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JEnumEnumTest extends AnyFlatSpec with OptionValues with Matchers {
  import JEnumEnum._

  behavior of "JEnumEnum"

  it should "znum jnums" in {
    import scalaz.syntax.enum._

    TimeUnit.NANOSECONDS.succ should equal(TimeUnit.MICROSECONDS)
    TimeUnit.NANOSECONDS.predx should equal(None)
  }
}