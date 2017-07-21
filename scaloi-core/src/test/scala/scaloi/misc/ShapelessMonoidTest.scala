package scaloi.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.std.iterable._
import scalaz.syntax.foldable._

class ShapelessMonoidTest extends FlatSpec with OptionValues with Matchers {
  import ShapelessMonoidTest._

  behavior of "ShapelessMonoid"

  it should "provide monoid evidence for case classes with monoidal types" in {
    import scalaz.Monoid

    import scalaz.std.anyVal._ // Int monoid
    import scalaz.std.string._ // String monoid
    import ShapelessMonoid._ // Product monoid

    implicit val someEv = Monoid[SomePojo]
    someEv.zero should equal(SomePojo("", 0))
    List(SomePojo("a", 1), SomePojo("b", 2)).suml should equal(
      SomePojo("ab", 3))
  }

}

object ShapelessMonoidTest {
  case class SomePojo(string: String, int: Int)
}
