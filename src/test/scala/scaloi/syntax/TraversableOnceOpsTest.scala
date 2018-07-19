package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class TraversableOnceOpsTest extends FlatSpec with OptionValues with Matchers {
  import TraversableOnceOps._

  behavior of "TraversableOnceOpsTest"

  it should "findMap values that are findable" in {
    import scalaz.syntax.std.boolean._

    List(1, 2, 3).findMap(i => (i == 2).option(i)).value shouldEqual 2
  }

  it should "findMap nothing where nothing is found" in {
    List(1, 2, 3).findMap(i => None) shouldEqual None
  }

  it should "findMap the first match" in {
    import scalaz.syntax.std.boolean._

    var count = 0
    List(1, 2, 2, 3).findMap(i => { count = count + 1; (i == 2).option(i) }).value shouldEqual 2
    count shouldEqual 2
  }

  it should "findMap in infinity" in {
    Stream.continually(1).findMap(i => Some(i)) shouldEqual Some(1)
  }
}
