package scaloi.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scalaz.std.anyVal.intInstance
import scalaz.std.iterable._
import scalaz.syntax.foldable._

class SemigroupsTest extends FlatSpec with OptionValues with Matchers {
  behavior of "Semigroups"

  it should "provide min/max semigroup evidence" in {
    List(1, 3, 2).suml1Opt(Semigroups.maxSemigroup[Int]) should equal(Some(3))
    List(1, 3, 2).suml1Opt(Semigroups.minSemigroup[Int]) should equal(Some(1))
  }

}
