package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class FoldableOpsTest extends FlatSpec with OptionValues with Matchers {
  import foldable._

  behavior of "FoldableOps"

  it should "findMap values that are findable" in {
    import scalaz.std.list._
    import scalaz.syntax.std.boolean._

    List(1, 2, 3).findMap(i => (i == 2).option(i)).value should equal(2)
  }

  it should "findMap nothing where nothing is found" in {
    import scalaz.std.list._

    List(1, 2, 3).findMap(i => None) should equal(None)
  }

  it should "findMap the first match" in {
    import scalaz.std.list._
    import scalaz.syntax.std.boolean._

    var count = 0
    List(1, 2, 2, 3).findMap(i => { count = count + 1 ; (i == 2).option(i) }).value should equal(2)
    count should equal(2)
  }

  it should "fold map, but flatly so" in {
    import scalaz.std.list._
    import scalaz.std.option._
    import scalaz.std.string._

    (1 to 10).toList.flatFoldMap {
      i => Some(i).filter(_ % 2 == 0).map(_.toString)
    } should equal ("246810")
  }

  it should "fold to maps" in {
    import scalaz.std.list._

    List(1, 2, 3).foldToMap(i => i -> i * 2) shouldEqual Map(1 -> 2, 2 -> 4, 3 -> 6)
  }
}
