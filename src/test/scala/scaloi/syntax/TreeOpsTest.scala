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
  import TreeOps._

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
}
