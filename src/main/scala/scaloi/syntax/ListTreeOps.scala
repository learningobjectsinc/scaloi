package scaloi
package syntax
import scaloi.data.ListTree

final class ListTreeOps[A](private val self: A) extends AnyVal {
  def listLeaf: ListTree[A] = ListTree.Leaf(self)
  def listNode(subForest: ListTree[A]*): ListTree[A] = ListTree.Node(self, subForest.toList)

}

object ListTreeOps extends ToListTreeOps

trait ToListTreeOps {
  import language.implicitConversions

  @inline implicit final def listTreeOps[A](self: A): ListTreeOps[A] =
    new ListTreeOps[A](self)

}
