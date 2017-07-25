package scaloi.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scalaz.std.anyVal.intInstance
import scalaz.std.iterable._
import scalaz.syntax.foldable._
import scalaz.syntax.std.boolean._

class SemigroupsTest extends FlatSpec with OptionValues with Matchers {
  import SemigroupsTest._

  behavior of "Semigroups"

  it should "provide min/max semigroup evidence" in {
    List(1, 3, 2).suml1Opt(Semigroups.maxSemigroup[Int]) should equal(Some(3))
    List(1, 3, 2).suml1Opt(Semigroups.minSemigroup[Int]) should equal(Some(1))
  }

  it should "capture the first throwable" in {
    import Semigroups.throwableSemiGroup

    import scalaz.std.anyVal.intInstance
    import scalaz.syntax.either._

    // The throwableSemiGroup captures the first failure in a sequence
    // of disjunctions.

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

    // However, it doesn't fail fast

    var state = 0
    List(1, 2, 3) foldMap { i =>
      state = state + i
      IntEx(i).asInstanceOf[Throwable].left[Int]
    } should equal(IntEx(1).left)
    state should equal(6)
  }

}

object SemigroupsTest {
  case class IntEx(value: Int) extends Exception(value.toString)
}