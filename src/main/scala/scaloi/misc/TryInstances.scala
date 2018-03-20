package scaloi.misc

import scala.util.Try
import scalaz.Applicative

trait TryInstances {
  implicit final val tryInstance = new Applicative[Try] {
    override def point[A](a: => A): Try[A]                          = Try(a)
    override def ap[A, B](fa: => Try[A])(f: => Try[A => B]): Try[B] = fa.flatMap(a => f.map(x => x(a)))
  }
}

object TryInstances extends TryInstances
