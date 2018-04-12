package scaloi.misc

import scala.util.Try
import scalaz.{Applicative, Monoid}
import scalaz.std.list._
import scalaz.syntax.foldable._

trait TryInstances {
  implicit final val tryInstance = new Applicative[Try] {
    override def point[A](a: => A): Try[A]                          = Try(a)
    override def ap[A, B](fa: => Try[A])(f: => Try[A => B]): Try[B] = fa.flatMap(a => f.map(x => x(a)))
  }

  def tryListAppend[A](fas: Try[List[A]], fbs: => Try[List[A]]): Try[List[A]] =
  fas.flatMap(as => fbs.map(bs => as ::: bs))
  implicit def tryListMonoid[A]: Monoid[Try[List[A]]] = Monoid.instance(tryListAppend, Try(List.empty))

  implicit class TryIterableOps[A](as: List[A]) {
    def traverseTryListFF[B](f: A => Try[B]) = as.foldMap(a => f(a).map(b => List(b)))
  }
}

object TryInstances extends TryInstances
