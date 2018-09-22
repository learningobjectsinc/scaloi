package scaloi
package syntax

import scalaz.StrictTree
import scalaz.StrictTree.Node
import scalaz.std.vector._
import scalaz.syntax.std.boolean._
import scaloi.syntax.FoldableOps._

final class StrictTreeOps[A](private val self: StrictTree[A]) extends AnyVal {

  /** A catamorphism over a tree.
    *
    * `f` is invoked with the label of the root of `self` and with the result
    * of folding `self`'s children with `f`.
    *
    * @note this may stack-overflow on very large trees.
    */
  def foldTree[B](f: (A, Vector[B]) => B): B = {
    def loop(current: StrictTree[A]): B = {
      f(current.rootLabel, current.subForest.map(loop))
    }
    loop(self)
  }

  /** A top-down histomorphism over a tree.
    *
    * The tree is mapped with a function `f` that that can draw from
    * the root label of each node and the mapped values of ancestor
    * nodes.
    */
  def tdhisto[B](f: (List[B], A) => B): StrictTree[B] = {
    def loop(tree: StrictTree[A], ancestors: List[B]): StrictTree[B] = {
      val b = f(ancestors, tree.rootLabel)
      Node(b, tree.subForest.map(loop(_, b :: ancestors)))
    }
    loop(self, Nil)
  }

  /** Select the `ix`th subtree of this tree, if it exists. */
  def get(ix: Int): Option[StrictTree[A]] = self.subForest.lift.apply(ix)

  def findParents(f: A => Boolean): Option[List[A]] = {
    def find(tree: StrictTree[A], parents: List[A]): Option[List[A]] = {
      val path = tree.rootLabel :: parents
      f(tree.rootLabel) option path orElse tree.subForest.findMap(find(_, path))
    }
    find(self, Nil)
  }

  /** Zip the tree's elements with their depth in the tree. */
  def zipWithDepth: StrictTree[(A, Int)] = {
    def loop(node: StrictTree[A], depth: Int): StrictTree[(A, Int)] = node match {
      case Node(content, children) =>
        Node((content, depth), children.map(loop(_, 1 + depth)))
    }
    loop(self, 0)
  }
}

object StrictTreeOps extends ToStrictTreeOps

trait ToStrictTreeOps {
  import language.implicitConversions

  @inline implicit final def ToStrictTreeOps[A](self: StrictTree[A]): StrictTreeOps[A] =
    new StrictTreeOps[A](self)

}
