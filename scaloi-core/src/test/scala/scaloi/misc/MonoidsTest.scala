package scaloi.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.std.iterable._
import scalaz.syntax.foldable._
import scalaz.syntax.std.boolean._

class MonoidsTest extends FlatSpec with OptionValues with Matchers {
  import MonoidsTest._

  it should "capture only first failure" in {
    import Monoids._

    import scalaz.std.anyVal.intInstance

    // The failFastDisjunctionMonoid looks like the throwableSemiGroup
    // with scalaz.DisjunctionInstances.DisjunctionMonoid

    List(1, 2, 3) foldMap { i =>
      i.rightFF[Throwable]
    } should equal(6.rightFF)

    List(1, 2, 3) foldMap { i =>
      intEx(i).leftFF[Int]
    } should equal(intEx(1).leftFF)

    List(1, 2, 3) foldMap { i =>
      FailFast((i == 1) either i or intEx(i))
    } should equal(intEx(2).leftFF)

    List(1, 2, 3) foldMap { i =>
      FailFast((i != 1) either i or intEx(i))
    } should equal(intEx(1).leftFF)

    // However, it fails fast

    var state = 0
    List(1, 2, 3) foldMap { i =>
      state = state + i
      intEx(i).leftFF[Int]
    } should equal(intEx(1).leftFF)
    state should equal(1)
  }

}

object MonoidsTest {
  case class IntEx(value: Int) extends Exception(value.toString)
  def intEx(value: Int): Throwable = IntEx(value)
}