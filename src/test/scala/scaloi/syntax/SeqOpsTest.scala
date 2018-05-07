package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class SeqOpsTest extends FlatSpec with OptionValues with Matchers {
  import SeqOps._

  behavior of "SeqOps"

  it should "count seqs" in {
    Stream.fill(3)(0).hasSize(3) should be(true)
    Stream.continually(0).hasSize(3) should be(false)
  }

  it should "group by key and map values" in {
    case class Foo(key: String, value: Int)
    val foos = Seq(
      Foo("First", 1),
      Foo("First", 2),
      Foo("Second", 3)
    )
    val groupedFoos: Map[String, Seq[Int]] = foos.groupMap(_.key, _.value)
    groupedFoos("First").size shouldBe 2
    groupedFoos("First") should contain allOf (1, 2)
    groupedFoos("Second").size shouldBe 1
    groupedFoos("Second") should contain(3)
  }
}
