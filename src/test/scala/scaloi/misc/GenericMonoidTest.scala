package scaloi.misc

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scalaz.std.iterable._
import scalaz.syntax.foldable._

class GenericMonoidTest extends AnyFlatSpec with OptionValues with Matchers {
  import GenericMonoidTest._

  behavior of "ShapelessMonoid"

  it should "provide monoid evidence for case classes with monoidal types" in {
    import GenericMonoid._
    import scalaz.Monoid

    import scalaz.std.anyVal._ // Int monoid
    import scalaz.std.string._ // String monoid
    import GenericMonoid._ // Product monoid

    implicit val someEv = Monoid[SomePojo]
    someEv.zero should equal(SomePojo("", 0))
    List(SomePojo("a", 1), SomePojo("b", 2)).suml should equal(
      SomePojo("ab", 3))
  }

  it should "allow for explicit summonation of the implicit derivation" in {
    import scalaz.Monoid

    import scalaz.std.anyVal._ // Int monoid
    import scalaz.std.string._ // String monoid

    /* this instance is not used */
    implicit val crapMonoid: Monoid[SomePojo] =
      Monoid.instance((s, p) => SomePojo("", 0), SomePojo("", 0))

    val decentMonoid = GenericMonoid[SomePojo]()

    decentMonoid.append(SomePojo("fix", 12), SomePojo("bax", 21)) should equal(
      SomePojo("fixbax", 33)
    )
  }

}

object GenericMonoidTest {
  case class SomePojo(string: String, int: Int)
}
