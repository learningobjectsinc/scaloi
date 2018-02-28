package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class SeqOpsTest extends FlatSpec with OptionValues with Matchers {
  import SeqOps._

  behavior of "SeqOps"

  it should "count seqs" in {
    Stream.fill(3)(0).hasSize(3) should be(true)
    Stream.continually(0).hasSize(3) should be(false)
  }
}
