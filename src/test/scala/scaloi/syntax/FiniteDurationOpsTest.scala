package scaloi.syntax

import java.util.concurrent.TimeUnit

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class FiniteDurationOpsTest extends FlatSpec with OptionValues with Matchers {
  import finiteDuration._

  behavior of "FiniteDurationOps"



  it should "javaficate" in {
    import java.time.Duration

    0.millis.asJava should equal(Duration.ZERO)
    16.millis.asJava should equal(Duration.ofMillis(16))
    1000.millis.asJava should equal(Duration.ofSeconds(1))
    (-5).seconds.asJava should equal(Duration.ofMillis(-5000))
    3041.nanos.asJava should equal(Duration.ofNanos(3041))
    (-4301).nanos.asJava should equal(Duration.ofNanos(-4301))
    (1340.seconds + 4103.nanos).asJava should equal(
      Duration.ofSeconds(1340, 4103))
    (-(1340.seconds + 4103.nanos)).asJava should equal(
      Duration.ofSeconds(1340, 4103).negated())
  }

  it should "stringify humanly" in {
    0.millis.toHumanString should equal("no time at all")
    1.millis.toHumanString should equal("1 millisecond")
    1001.millis.toHumanString should equal("1 second, 1 millisecond")
    60001.millis.toHumanString should equal("1 minute")
    60001.millis.toFiniteDuration(TimeUnit.MINUTES) should equal(1.minute)
  }

}
