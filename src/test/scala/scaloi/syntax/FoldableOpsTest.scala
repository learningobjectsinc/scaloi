package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class FoldableOpsTest extends FlatSpec with OptionValues with Matchers {
  import FoldableOps._

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
}
