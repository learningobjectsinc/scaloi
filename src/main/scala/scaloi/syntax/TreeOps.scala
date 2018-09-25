package scaloi
package syntax

import scalaz.Tree.Node
import scalaz.syntax.std.boolean._
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
      val bs     = Need(ancestors)
      lazy val b = f(bs.value, tree.rootLabel)
      Node(b, tree.subForest.map(loop(_, b #:: bs.value)))
    }
    loop(self, Stream.empty)
  }

  /** Left-biased tree filter. Errs on the side of exclusivity: If an ancestor
    * is excluded then so too will be its descendants.
    *
    * @param f the predicate
    * @return the resulting filtered tree, if any
    */
  def filtl(f: A => Boolean): Option[Tree[A]] = {
    def loop(tree: Tree[A]): Option[Tree[A]] = tree match {
      case Node(content, subForest) =>
        f(content) option Node(content, subForest.flatMap(loop))
    }
    loop(self)
  }

  /** Right-biased tree filter. Errs on the side of inclusivity: If a descendant
    * is included then so too will be its ancestors.
    *
    * @param f the predicate
    * @return the resulting filtered tree, if any
    */
  def filtr(f: A => Boolean): Option[Tree[A]] = {
    def loop(tree: Tree[A]): Option[Tree[A]] = tree match {
      case Node(content, subForest) =>
        lazy val filteredForest = subForest.flatMap(loop)
        (f(content) || filteredForest.nonEmpty) option Node(content, filteredForest)
    }
    loop(self)
  }

  /** Select the `ix`th subtree of this tree, if it exists. */
  def get(ix: Int): Option[Tree[A]] = self.subForest.lift.apply(ix)

  /** Map the values in this tree along with their position relative to their
    * parent's sub-forest.
    */
  def mapWithIndices[B](f: (Int, A) => B): Tree[B] =
    self.loc.coflatMap { here =>
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
