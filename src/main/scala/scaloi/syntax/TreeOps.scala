package scaloi
package syntax

import scalaz.Tree.Node
import scalaz.syntax.comonad._
import scalaz.{Need, Tree}

final class TreeOps[A](private val self: Tree[A]) extends AnyVal {

  /** A catamorphism over a tree.
    *
    * `f` is invoked with the label of the root of `self` and with the result
    * of folding `self`'s children with `f`.
    *
    * @note this may stack-overflow on very large trees.
    */
  def foldTree[B](f: (A, => Stream[B]) => B): B = {
    def loop(current: Tree[A]): B = {
      lazy val folded = current.subForest.map(loop) // `Stream` not lazy enough
      f(current.rootLabel, folded)
    }

    loop(self)
  }

  /** A top-down histomorphism over a tree.
    *
    * The tree is mapped with a function `f` that that can draw from
    * the root label of each node and the mapped values of ancestor
    * nodes.
    */
  def tdhisto[B](f: (=> Stream[B], A) => B): Tree[B] = {
    def loop(tree: Tree[A], ancestors: => Stream[B]): Tree[B] = {
      val bs = Need(ancestors)
      lazy val b = f(bs.value, tree.rootLabel)
      Node(b, tree.subForest.map(loop(_, b #:: bs.value)))
    }
    loop(self, Stream.empty)
  }

  /** Select the `ix`th subtree of this tree, if it exists. */
  def get(ix: Int): Option[Tree[A]] = self.subForest.lift.apply(ix)

  /** Map the values in this tree along with their position relative to their
    * parent's sub-forest.
    */
  def mapWithIndices[B](f: (Int, A) => B): Tree[B] = self.loc.coflatMap {
    here =>
      val ix = here.lefts.size
      f(ix, here.tree.rootLabel)
  }.tree

}

object TreeOps extends ToTreeOps

trait ToTreeOps {
  import language.implicitConversions

  @inline implicit final def ToTreeOps[A](self: Tree[A]): TreeOps[A] =
    new TreeOps[A](self)

  // help avoid name clashes
  @inline implicit final def ToZTreeOps[A](self: A): scalaz.syntax.TreeOps[A] =
    new scalaz.syntax.TreeOps[A](self)

}
