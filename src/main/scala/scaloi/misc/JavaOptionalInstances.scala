package scaloi.misc

import java.{util => ju}

import scalaz.{-\/, Alternative, Monad, Optional, PlusEmpty, \/, \/-}

/** ju.Optional is a passable scalaz.Optional, scalaz.Monad, and scalaz.PlusEmpty. */
trait JavaOptionalInstances {

  implicit final val javaOptionalInstance: Monad[ju.Optional] with PlusEmpty[ju.Optional] with Alternative[ju.Optional] with Optional[ju.Optional] =
    new Monad[ju.Optional] with PlusEmpty[ju.Optional] with Alternative[ju.Optional] with Optional[ju.Optional] {
      def empty[A]: ju.Optional[A] = ju.Optional.empty[A]

      def plus[A](a: ju.Optional[A], b: => ju.Optional[A]): ju.Optional[A] =
        if (a.isPresent) a else b

      def point[A](a: => A): ju.Optional[A] = ju.Optional.of(a)

      def bind[A, B](fa: ju.Optional[A])(f: (A) => ju.Optional[B]): ju.Optional[B] =
        fa flatMap f.apply _

      def pextract[B, A](fa: ju.Optional[A]): ju.Optional[B] \/  A =
        if (fa.isPresent) \/-(fa.get) else -\/(ju.Optional.empty[B])

    }

}
object JavaOptionalInstances extends JavaOptionalInstances
