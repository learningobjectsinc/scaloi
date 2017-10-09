package scaloiz.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scalaz.std.iterable._
import scalaz.syntax.foldable._
import scalaz.syntax.std.boolean._

class MonoidsTest extends FlatSpec with OptionValues with Matchers {
  import MonoidsTest._

  it should "capture only first failure" in {
    import Monoids.failFastDisjunctionMonoid

    import scalaz.std.anyVal.intInstance
    import scalaz.syntax.either._

    // The failFastDisjunctionMonoid looks like the throwableSemiGroup
    // with scalaz.DisjunctionInstances.DisjunctionMonoid

    List(1, 2, 3) foldMap { i =>
      i.right[Throwable]
    } should equal(6.right)
    List(1, 2, 3) foldMap { i =>
      IntEx(i).asInstanceOf[Throwable].left[Int]
    } should equal(IntEx(1).left)
    List(1, 2, 3) foldMap { i =>
      (i == 1) either i or IntEx(i).asInstanceOf[Throwable]
    } should equal(IntEx(2).left)
    List(1, 2, 3) foldMap { i =>
      (i != 1) either i or IntEx(i).asInstanceOf[Throwable]
    } should equal(IntEx(1).left)

    // However, it fails fast

    var state = 0
    List(1, 2, 3) foldMap { i =>
      state = state + i
      IntEx(i).asInstanceOf[Throwable].left[Int]
    } should equal(IntEx(1).left)
    state should equal(1)
  }

}

object MonoidsTest {
  case class IntEx(value: Int) extends Exception(value.toString)
}