package scaloi
package data

import scalaz._

import scala.collection.mutable
import scalaz.std.list.{listInstance, listMonoid}
import scala.language.implicitConversions

/**
  *
  * @param rootLabel The label at the root of this tree.
  * @param subForest The child nodes of this tree.
  * @tparam A
  */
case class ListTree[A](
  rootLabel: A,
  subForest: List[ListTree[A]]
) {

  import ListTree._

  /**
    * Run a bottom-up algorithm.
    *
    * This is the framework for several stackless methods, such as map.
    *
    * @param reduce is a function from a label and its mapped children to the new result.
    */
  private[data] def runBottomUp[B](
    reduce: A => mutable.Buffer[B] => B
  ): B = {
    val root = BottomUpStackElem[A, B](None, this)
    val stack = mutable.Stack[BottomUpStackElem[A, B]](root)

    while (stack.nonEmpty) {
      val here = stack.head
      if (here.hasNext) {
        val child = here.next()
        val nextStackElem = BottomUpStackElem[A, B](Some(here), child)
        stack.push(nextStackElem)
      } else {
        //The "here" node is completed, so add its result to its parents completed children.
        val result = reduce(here.rootLabel)(here.mappedSubForest)
        here.parent.foreach(_.mappedSubForest += result)
        stack.pop()
      }
    }

    reduce(root.rootLabel)(root.mappedSubForest)
  }

  /** Maps the elements of the ListTree into a Monoid and folds the resulting ListTree. */
  def foldMap[B: Monoid](f: A => B): B =
    runBottomUp(foldMapReducer(f))

  def foldRight[B](z: B)(f: (A, => B) => B): B =
    Foldable[List].foldRight(flatten, z)(f)

  /** A 2D String representation of th«is ListTree. */
  def drawTree(implicit sh: Show[A]): String = {
    toTree.drawTree
  }

  /** A histomorphic transform. Each element in the resulting tree
    * is a function of the corresponding element in this tree
    * and the histomorphic transform of its children.
    */
  def scanr[B](g: (A, List[ListTree[B]]) => B): ListTree[B] =
    runBottomUp(scanrReducer(g))

  /** Pre-order traversal. */
  def flatten: List[A] = {
    val stack = mutable.Stack(this)

    val result = mutable.Buffer.empty[A]

    while (stack.nonEmpty) {
      val popped = stack.pop()
      result += popped.rootLabel
      popped.subForest.reverseIterator.foreach(stack.push)
    }

    result.toList
  }

  def size: Int = {
    val stack = mutable.Stack(this.subForest)

    var result = 1

    while (stack.nonEmpty) {
      val popped = stack.pop()
      result += popped.size
      stack.pushAll(popped.map(_.subForest))
    }

    result
  }

  /** Breadth-first traversal. */
  def levels: List[List[A]] = {
    val f = (s: List[ListTree[A]]) => {
      Foldable[List].foldMap(s)((_: ListTree[A]).subForest)
    }
    List.iterate(List(this), depth)(f) map { _ map (_.rootLabel) }
  }

  def depth: Int = {
    var level = List(this)
    var result = 0

    while (level.nonEmpty) {
      level = Foldable[List].foldMap(level)(_.subForest)
      result += 1
    }

    result
  }

  def toTree: Tree[A] = {
    Tree.Node[A](rootLabel, subForest.toStream.map(_.toTree))
  }

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: ListTree[A] => B): ListTree[B] = unfoldTree(this)(t => (f(t), t.subForest))

  def foldNode[Z](f: A => List[ListTree[A]] => Z): Z =
    f(rootLabel)(subForest)

  def map[B](f: A => B): ListTree[B] = {
    runBottomUp(mapReducer(f))
  }

  def flatMap[B](f: A => ListTree[B]): ListTree[B] = {
    runBottomUp(flatMapReducer(f))
  }

  def traverse1[G[_] : Apply, B](f: A => G[B]): G[ListTree[B]] = {
    val G = Apply[G]

    subForest match {
      case Nil => G.map(f(rootLabel))(Leaf(_))
      case x :: xs => G.apply2(f(rootLabel), NonEmptyList.nel(x, IList.fromList(xs)).traverse1(_.traverse1(f))) {
        case (h, t) => Node(h, t.list.toList)
      }
    }
  }

  def zip[B](b: ListTree[B]): ListTree[(A, B)] = {
    val root = ZipStackElem[A, B](None, this, b)
    val stack = mutable.Stack[ZipStackElem[A, B]](root)

    while (stack.nonEmpty) {
      val here = stack.head
      if (here.hasNext) {
        val (childA, childB) = here.next()
        val nextStackElem = ZipStackElem[A, B](Some(here), childA, childB)
        stack.push(nextStackElem)
      } else {
        //The "here" node is completed, so add its result to its parents completed children.
        val result = ListTree((here.a.rootLabel, here.b.rootLabel), here.mappedSubForest.toList)
        here.parent.foreach(_.mappedSubForest += result)
        stack.pop()
      }
    }

    ListTree((rootLabel, b.rootLabel), root.mappedSubForest.toList)
  }

  /**
    * This implementation is 24x faster than the trampolined implementation for ListTreeTestJVM's hashCode test.
    *
    * @return
    */
  override def hashCode(): Int = {
    runBottomUp(hashCodeReducer)
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case other: ListTree[A] =>
        ListTree.badEqInstance[A].equal(this, other)
      case _ =>
        false
    }
  }

  // from scaloi syntax for other trees

  /** A catamorphism over a tree.
    *
    * `f` is invoked with the label of the root of `self` and with the result
    * of folding `self`'s children with `f`.
    *
    * @note this may stack-overflow on very large trees.
    */
  def foldTree[B](f: (A, List[B]) => B): B = {
    def loop(current: ListTree[A]): B = {
      f(current.rootLabel, current.subForest.map(loop))
    }
    loop(this)
  }

  /** A top-down histomorphism over a tree.
    *
    * The tree is mapped with a function `f` that that can draw from
    * the root label of each node and the mapped values of ancestor
    * nodes.
    */
  def tdhisto[B](f: (List[B], A) => B): ListTree[B] = {
    def loop(tree: ListTree[A], ancestors: List[B]): ListTree[B] = {
      val b = f(ancestors, tree.rootLabel)
      Node(b, tree.subForest.map(loop(_, b :: ancestors)))
    }
    loop(this, Nil)
  }

  /** Select the `ix`th subtree of this tree, if it exists. */
  def get(ix: Int): Option[ListTree[A]] = subForest.lift.apply(ix)

  /** Finds a node matching the given predicate and returns the
    * path from the matching node to the root. */
  def findPath(f: A => Boolean): Option[List[ListTree[A]]] = {
    import syntax.std.boolean._
    import scaloi.syntax.FoldableOps._
    def find(tree: ListTree[A], parents: List[ListTree[A]]): Option[List[ListTree[A]]] = {
      val path = tree :: parents
      f(tree.rootLabel) option path orElse tree.subForest.findMap(find(_, path))
    }
    find(this, Nil)
  }

  /** Zip the tree's elements with their depth in the tree. */
  def zipWithDepth: ListTree[(A, Int)] = {
    def loop(node: ListTree[A], depth: Int): ListTree[(A, Int)] = node match {
      case Node(content, children) =>
        Node((content, depth), children.map(loop(_, 1 + depth)))
    }
    loop(this, 0)
  }

}

sealed abstract class ListTreeInstances {
  implicit val listTreeInstance: Traverse1[ListTree] with Monad[ListTree] with Comonad[ListTree] with Align[ListTree] with Zip[ListTree] = new Traverse1[ListTree] with Monad[ListTree] with Comonad[ListTree] with Align[ListTree] with Zip[ListTree] {
    def point[A](a: => A): ListTree[A] = ListTree.Leaf(a)
    def cobind[A, B](fa: ListTree[A])(f: ListTree[A] => B): ListTree[B] = fa cobind f
    def copoint[A](p: ListTree[A]): A = p.rootLabel
    override def map[A, B](fa: ListTree[A])(f: A => B) = fa map f
    def bind[A, B](fa: ListTree[A])(f: A => ListTree[B]): ListTree[B] = fa flatMap f
    def traverse1Impl[G[_]: Apply, A, B](fa: ListTree[A])(f: A => G[B]): G[ListTree[B]] = fa traverse1 f
    override def foldRight[A, B](fa: ListTree[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
    override def foldMapRight1[A, B](fa: ListTree[A])(z: A => B)(f: (A, => B) => B) = (fa.flatten.reverse: @unchecked) match {
      case h +: t => t.foldLeft(z(h))((b, a) => f(a, b))
    }
    override def foldLeft[A, B](fa: ListTree[A], z: B)(f: (B, A) => B): B =
      fa.flatten.foldLeft(z)(f)
    override def foldMapLeft1[A, B](fa: ListTree[A])(z: A => B)(f: (B, A) => B): B = fa.flatten match {
      case h +: t => t.foldLeft(z(h))(f)
    }
    override def foldMap[A, B](fa: ListTree[A])(f: A => B)(implicit F: Monoid[B]): B = fa foldMap f

    //This implementation is 14x faster than the trampolined implementation for ListTreeTestJVM's align test.
    override def alignWith[A, B, C](f: (\&/[A, B]) => C): (ListTree[A], ListTree[B]) => ListTree[C] = {
      (a, b) =>
        import ListTree.AlignStackElem
        val root = AlignStackElem[A, B, C](None, \&/(a, b))
        val stack = mutable.Stack(root)

        while (stack.nonEmpty) {
          val here = stack.head
          if (here.hasNext) {
            val nextChildren = here.next()
            val nextStackElem = AlignStackElem[A, B, C](Some(here), nextChildren)
            stack.push(nextStackElem)
          } else {
            //The "here" node is completed, so add its result to its parents completed children.
            val result = ListTree[C](f(here.trees.bimap(_.rootLabel, _.rootLabel)), here.mappedSubForest.toList)
            here.parent.foreach(_.mappedSubForest += result)
            stack.pop()
          }
        }

        ListTree(f(root.trees.bimap(_.rootLabel, _.rootLabel)), root.mappedSubForest.toList)
    }

    override def zip[A, B](a: => ListTree[A], b: => ListTree[B]): ListTree[(A, B)] = {
      a.zip(b)
    }
  }

  implicit def treeEqual[A](implicit A0: Equal[A]): Equal[ListTree[A]] =
    new ListTreeEqual[A] { def A = A0 }

  implicit def treeOrder[A](implicit A0: Order[A]): Order[ListTree[A]] =
    new Order[ListTree[A]] with ListTreeEqual[A] {
      import std.list.listOrder
      def A = A0
      override def order(x: ListTree[A], y: ListTree[A]) =
        A.order(x.rootLabel, y.rootLabel) match {
          case Ordering.EQ =>
            Order[List[ListTree[A]]].order(x.subForest, y.subForest)
          case x => x
        }
    }



  /* TODO
  def applic[A, B](f: ListTree[A => B]) = a => ListTree.node((f.rootLabel)(a.rootLabel), implicitly[Applic[newtypes.ZipVector]].applic(f.subForest.map(applic[A, B](_)).?)(a.subForest ?).value)
   */
}

object ListTree extends ListTreeInstances {
  /**
   * Node represents a tree node that may have children.
   *
   * You can use Node for tree construction or pattern matching.
   */
  object Node {
    def apply[A](root: A, forest: List[ListTree[A]]): ListTree[A] = {
      ListTree[A](root, forest)
    }

    def unapply[A](t: ListTree[A]): Option[(A, List[ListTree[A]])] = Some((t.rootLabel, t.subForest))
  }

  /**
   *  Leaf represents a tree node with no children.
   *
   *  You can use Leaf for tree construction or pattern matching.
   */
  object Leaf {
    def apply[A](root: A): ListTree[A] = {
      Node(root, Nil)
    }

    def unapply[A](t: ListTree[A]): Option[A] = {
      t match {
        case Node(root, List()) =>
          Some(root)
        case _ =>
          None
      }
    }
  }

  def unfoldForest[A, B](s: List[A])(f: A => (B, List[A])): List[ListTree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, List[A])): ListTree[B] =
    f(v) match {
      case (a, bs) => Node(a, unfoldForest(bs)(f))
    }

  //Only used for .equals.
  private def badEqInstance[A] = new ListTreeEqual[A] {
    override def A: Equal[A] = new Equal[A] {
      override def equal(a1: A, a2: A): Boolean = a1.equals(a2)
    }
  }

  /**
    * This implementation is 16x faster than the trampolined implementation for ListTreeTestJVM's scanr test.
    */
  private def scanrReducer[A, B](
    f: (A, List[ListTree[B]]) => B
  )(rootLabel: A
  )(subForest: mutable.Buffer[ListTree[B]]
  ): ListTree[B] = {
    val subForestList = subForest.toList
    ListTree[B](f(rootLabel, subForestList), subForestList)
  }

  /**
    * This implementation is 10x faster than mapTrampoline for ListTreeTestJVM's map test.
    */
  private def mapReducer[A, B](
    f: A => B
  )(rootLabel: A
  )(subForest: scala.collection.Seq[ListTree[B]]
  ): ListTree[B] = {
    ListTree[B](f(rootLabel), subForest.toList)
  }

  /**
    * This implementation is 9x faster than flatMapTrampoline for ListTreeTestJVM's flatMap test.
    */
  private def flatMapReducer[A, B](
    f: A => ListTree[B]
  )(root: A
  )(subForest: scala.collection.Seq[ListTree[B]]
  ): ListTree[B] = {
    val ListTree(rootLabel0, subForest0) = f(root)
    ListTree(rootLabel0, subForest0 ++ subForest)
  }

  /**
    * This implementation is 9x faster than the trampolined implementation for ListTreeTestJVM's foldMap test.
    */
  private def foldMapReducer[A, B: Monoid](
    f: A => B
  )(rootLabel: A
  )(subForest: mutable.Buffer[B]
  ): B = {
    val mappedRoot = f(rootLabel)
    val foldedForest = Foldable[List].fold[B](subForest.toList)

    Monoid[B].append(mappedRoot, foldedForest)
  }

  private def hashCodeReducer[A](root: A)(subForest: scala.collection.Seq[Int]): Int = {
    root.hashCode ^ subForest.hashCode
  }

  private case class BottomUpStackElem[A, B](
    parent: Option[BottomUpStackElem[A, B]],
    tree: ListTree[A]
  ) extends Iterator[ListTree[A]] {
    private[this] val subIterator = tree.subForest.iterator

    def rootLabel = tree.rootLabel

    val mappedSubForest: mutable.Buffer[B] = mutable.Buffer.empty

    override def hasNext: Boolean = subIterator.hasNext

    override def next(): ListTree[A] = subIterator.next()
  }

  private case class ZipStackElem[A, B](
    parent: Option[ZipStackElem[A, B]],
    a: ListTree[A],
    b: ListTree[B]
  ) extends Iterator[(ListTree[A], ListTree[B])] {
    private[this] val zippedSubIterator =
      a.subForest.iterator.zip(b.subForest.iterator)

    val mappedSubForest: mutable.Buffer[ListTree[(A, B)]] = mutable.Buffer.empty

    override def hasNext: Boolean = zippedSubIterator.hasNext

    override def next(): (ListTree[A], ListTree[B]) = zippedSubIterator.next()
  }

  private[data] case class AlignStackElem[A, B, C](
    parent: Option[AlignStackElem[A, B, C]],
    trees: \&/[ListTree[A], ListTree[B]]
  ) extends Iterator[\&/[ListTree[A], ListTree[B]]] {
    private[this] val iterators =
      trees.bimap(_.subForest.iterator, _.subForest.iterator)

    val mappedSubForest: mutable.Buffer[ListTree[C]] = mutable.Buffer.empty

    def whichHasNext: \&/[Boolean, Boolean] =
      iterators.bimap(_.hasNext, _.hasNext)

    override def hasNext: Boolean =
      whichHasNext.fold(identity, identity, _ || _)

    override def next(): \&/[ListTree[A], ListTree[B]] =
      whichHasNext match {
        case \&/(true, true) =>
          iterators.bimap(_.next(), _.next())

        case \&/(true, false) | \&/.This(true) =>
          \&/.This(iterators.onlyThis.get.next())

        case \&/(false, true) | \&/.That(true) =>
          \&/.That(iterators.onlyThat.get.next())

        case _ =>
          throw new NoSuchElementException("reached iterator end")
      }
  }

  implicit def ToListTreeUnzip[A1, A2](root: ListTree[(A1, A2)]): ListTreeUnzip[A1, A2] =
    new ListTreeUnzip[A1, A2](root)

}

private trait ListTreeEqual[A] extends Equal[ListTree[A]] {
  def A: Equal[A]

  private case class EqualStackElem(
    a: ListTree[A],
    b: ListTree[A]
  ) {
    val aSubIterator =
      a.subForest.iterator

    val bSubIterator =
      b.subForest.iterator
  }

  //This implementation is 4.5x faster than the trampolined implementation for ListTreeTestJVM's equal test.
  override final def equal(a1: ListTree[A], a2: ListTree[A]): Boolean = {
    val root = EqualStackElem(a1, a2)
    val stack = mutable.Stack[EqualStackElem](root)

    while (stack.nonEmpty) {
      val here = stack.head
      if (A.equal(here.a.rootLabel, here.b.rootLabel)) {
        val aNext = here.aSubIterator.hasNext
        val bNext = here.bSubIterator.hasNext
        (aNext, bNext) match {
          case (true, true) =>
            val childA = here.aSubIterator.next()
            val childB = here.bSubIterator.next()
            val nextStackElem = EqualStackElem(childA, childB)
            stack.push(nextStackElem)
          case (false, false) =>
            stack.pop()
          case _ =>
            return false
        }
      } else return false
    }

    true
  }
}

final class ListTreeUnzip[A1, A2](private val root: ListTree[(A1, A2)]) extends AnyVal {
  private def unzipCombiner(rootLabel: (A1, A2))(accumulator: scala.collection.Seq[(ListTree[A1], ListTree[A2])]): (ListTree[A1], ListTree[A2]) = {
    (ListTree(rootLabel._1, accumulator.map(_._1).toList), ListTree(rootLabel._2, accumulator.map(_._2).toList))
  }

  /** Turns a tree of pairs into a pair of trees. */
  def unzip: (ListTree[A1], ListTree[A2]) = {
    root.runBottomUp[(ListTree[A1], ListTree[A2])](unzipCombiner)
  }
}
