package scaloi.syntax

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scaloi.test.ScaloiTest

class SeqOpsTest
    extends AnyFlatSpec
       with Matchers
       with OptionValues
       with ScaloiTest
{
  import seq._

  behaviour of "SeqOps"

  it should "count seqs" in {
    Stream.fill(3)(0).hasSize(3) should be(true)
    Stream.continually(0).hasSize(3) should be(false)
  }

  it should "map sequence values to their index" in {
    Seq("A", "B", "C").mapByIndex shouldBe Map(0 -> "A", 1 -> "B", 2 -> "C")
  }

}
