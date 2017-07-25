package scaloi.misc

import enumeratum.{Enum, EnumEntry}
import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scalaz.std.iterable._
import scalaz.syntax.foldable._

class ScalaZEnumTest extends FlatSpec with OptionValues with Matchers {
  import ScalaZEnumTest._

  behavior of "ScalaZEnum"

  it should "provide enum evidence" in {
    import scalaz.syntax.enum._

    Greeting.values.minimum should equal(Some(Greeting.Hi))
    Greeting.values.maximum should equal(Some(Greeting.Hello))

    Greeting.hi.succ should equal(Greeting.Ho)
    Greeting.hello.succ should equal(Greeting.Hi)
    Greeting.hi |-> Greeting.hello should equal(Greeting.values.toList)
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
