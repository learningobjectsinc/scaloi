package scaloi

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class MultiMapTest extends FlatSpec with Matchers with OptionValues {
  import MultiMap._

  behavior of "MultiMapOps"

  it should "add to multimaps" in {
    val mm = MultiMap
      .empty[Int, String]
      .add(1, "1")
      .add(2, "baz")
      .add(1 -> "zippo")
      .add(4, Set("fo", "ur"))

    mm.get(1).value should equal (Set("1", "zippo"))
    mm.get(2).value should equal (Set("baz"))
    mm.get(3) should be ('empty)
    mm.get(4).value should equal (Set("fo", "ur"))
  }

  it should "combine multimaps" in {
    val mm1 = MultiMap.empty[String, Int].add("x" -> 1, "y" -> 2)
    val mm2 = MultiMap.empty[String, Int].add("x" -> 3, "z" -> 4)

    val both = mm1.combine(mm2)
    both.size should be (3)
    both.keySet should equal (Set("x", "y", "z"))
    both.get("x").value should equal (Set(1, 3))
    both.get("y").value should equal (Set(2))
    both.get("z").value should equal (Set(4))
    both.get("zed") should be ('empty)
  }

  it should "chain multimaps" in {
    val mm1 = MultiMap.empty[Int, Symbol].add(4 -> 'four, 7 -> 'seven, 12 -> 'twelve)
    val mm2 = MultiMap.empty[Symbol, String].add('four -> "four", 'eight -> "eight")

    val chaint = mm1.chain(mm2)

    chaint.size should be (1)
    chaint.get(4).value should equal (Set("four"))
    chaint.get(7) should be ('empty)
    chaint.get(8) should be ('empty)
    chaint.get(12) should be ('empty)
  }

}
