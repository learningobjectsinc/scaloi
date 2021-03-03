package scaloi.misc

import enumeratum._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scalaz.std.iterable._
import scalaz.syntax.foldable._

class ScalaZEnumTest extends AnyFlatSpec with OptionValues with Matchers {
  import ScalaZEnumTest._

  behavior of "ScalaZEnum"

  it should "provide enum evidence" in {
    import scalaz.syntax.enum._

    Greeting.values.minimum should equal(Some(Greeting.Hi))
    Greeting.values.maximum should equal(Some(Greeting.Hello))

    Greeting.hi.succ should equal(Greeting.Ho)
    Greeting.hello.succ should equal(Greeting.Hi)
    Greeting.hi |-> Greeting.hello should equal(Greeting.values.toIList)
    Greeting.hi.predx should equal(None)
  }

}

object ScalaZEnumTest {
  sealed trait Greeting extends EnumEntry

  object Greeting extends Enum[Greeting] with ScalaZEnum[Greeting] {
    val values = findValues
    case object Hi extends Greeting
    case object Ho extends Greeting
    case object Hello extends Greeting

    val hi: Greeting = Hi
    val ho: Greeting = Ho
    val hello: Greeting = Hello
  }

}
