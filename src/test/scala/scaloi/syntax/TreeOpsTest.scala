package scaloi
package syntax

import org.scalatest.{FlatSpec, Matchers}
import scalaz.Tree

class TreeOpsTest
  extends FlatSpec
     with Matchers
     with test.ScaloiTest
{

  behaviour of "TreeOps"
  import tree._

  it should "catamorphize" in {
    def sumF(i: Int, children: => Stream[Int]) = i + children.sum

    1.leaf.foldTree(sumF) should equal (1)
    1.node(2.leaf, 3.leaf).foldTree(sumF) should equal (6)

    def countF(x: String, children: => Stream[Int]) = 1 + children.sum
    "".leaf.foldTree(countF) should equal (1)
    "asdf".node("ff".leaf, "q".node("a".leaf)).foldTree(countF) should equal (4)

    def leavesF(x: String, children: => Stream[List[String]]) =
      if (children.isEmpty) x :: Nil else children.flatten.toList
    "".leaf.foldTree(leavesF) should equal ("" :: Nil)
    "asdf".node("ff".leaf, "q".node("a".leaf)).foldTree(leavesF) should equal ("ff" :: "a" :: Nil)
  }

  it should "be zack safe" ignore {
    Stream.iterate(1.leaf)(t => 1.node(t, t)).apply(10000).foldTree[Int] {
      (here, children) => here + children.sum
    }
  }

  it should "top-down histomorph" in {
    val as = 1.node(2.node(3.leaf), 4.node(5.leaf, 6.leaf))
    val bs = 1.node(3.node(7.leaf), 5.node(11.leaf, 12.leaf)) // every node is the sum of a plus its ancestry in bs
    def f(bs: => Stream[Int], a: Int): Int = bs.fold(a)(_ + _)
    as.tdhisto(f).flatten shouldEqual bs.flatten
  }

  it should "filter" in {
    val as     = 1.node(2.node(3.leaf), 4.node(5.leaf, 6.leaf))
    val anyOdd = 1.node(2.node(3.leaf), 4.node(5.leaf))
    val allOdd = 1.leaf
    as.filtr(_ % 2 == 1).map(_.flatten) shouldEqual Some(anyOdd.flatten)
    as.filtl(_ % 2 == 1).map(_.flatten) shouldEqual Some(allOdd.flatten)
  }

  it should "rebuild a tree" in {
    import scalaz.std.anyVal._
    import scalaz.std.stream._
    import scalaz.syntax.foldable._
    val tree = 1.node(
      2.leaf,
      3.node(4.leaf, 5.leaf, 6.leaf),
      7.node(8.leaf),
    )
    val rebuilt = tree.rebuild[Int] {
      case (label, children) =>
        val subsum = children.foldMap(_.rootLabel)
        if (subsum % 2 == 0) (label + subsum).leaf
        else (label + subsum).node(children : _*)
    }
    rebuilt.levels should equal (
      36.node(
        2.leaf,
        18.node(4.leaf, 5.leaf, 6.leaf),
        15.leaf,
      ).levels
    )
  }

  it should "be able to assign indices to a tree" in {
    def indexMap[A](tree: Tree[A]): Map[A, Int] =
      tree.mapWithIndices((ix, a) => a -> ix).flatten.toMap

    indexMap("a".leaf) should be (Map("a" -> 0))
    indexMap("a".node(
      "b".leaf,
      "c".node(
        "d".leaf,
        "e".node("f".leaf),
        "g".node("h".leaf),
      ),
      "d".leaf,
    )) should be (Map(
      "a" -> 0,
      "b" -> 0,
      "c" -> 1,
      "d" -> 0,
      "e" -> 1,
      "f" -> 0,
      "g" -> 2,
      "h" -> 0,
      "d" -> 2,
    ))

  }
  it should "be able to zip with depth" in {
    "a"
      .node("b".node("c".node("d".leaf), "e".leaf), "f".node("g".leaf, "h".node("i".leaf)))
      .zipWithDepth
      .flatten shouldEqual ("a" -> 0)
      .node(("b" -> 1).node(("c" -> 2).node(("d" -> 3).leaf), ("e" -> 2).leaf),
            ("f" -> 1).node(("g" -> 2).leaf, ("h" -> 2).node(("i" -> 3).leaf)))
      .flatten
  }
}
