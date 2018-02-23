package scaloi.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.std.iterable._
import scalaz.syntax.foldable._

class TaggedMonoidTest extends FlatSpec with OptionValues with Matchers {
  behavior of "TaggedMonoid"

  it should "provide monoid evidence for longs with maximum" in {
    import scalaz.std.anyVal._
    import scalaz.{@@, Tags}
    // Tagged Long semigroup

    implicit val maxEv = TaggedMonoid[Tags.MaxVal](-1L)
    List(1L, 3L, 2L).map(Tags.MaxVal.apply).suml should equal(Tags.MaxVal(3L))
    List.empty[Long @@ Tags.MaxVal].suml should equal(Tags.MaxVal(-1L))
  }

  it should "provide monoid evidence for ints with minimum" in {
    import scalaz.std.anyVal._
    import scalaz.{@@, Tags}
    // Tagged Int semigroup

    implicit val minEv = TaggedMonoid[Tags.MinVal](100)
    List(4, 3, 9).map(Tags.MinVal.apply).suml should equal(Tags.MinVal(3))
    List.empty[Int @@ Tags.MinVal].suml should equal(Tags.MinVal(100))
  }
}
