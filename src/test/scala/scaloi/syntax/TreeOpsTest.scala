package scaloi
package syntax

import org.scalatest.{FlatSpec, Matchers}

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

}
