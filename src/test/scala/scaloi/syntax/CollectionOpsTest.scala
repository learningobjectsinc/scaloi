package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class CollectionOpsTest extends FlatSpec with OptionValues with Matchers {
  import CollectionOps._

  behavior of "CollectionOps"

  it should "criss crossingly" in {
    Seq(1, 2) × Seq('a', 'b') should contain allOf ((1,'a'), (2,'a'), (1,'b'), (2,'b'))

  }
}
