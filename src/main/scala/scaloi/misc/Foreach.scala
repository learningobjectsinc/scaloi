package scaloi.misc

import scalaz.Foldable
import scalaz.Id.Id

import scala.collection.GenTraversableOnce

/** Typeclass evidence for the ability to side-effectively iterate over a container type. */
trait Foreach[F[_]] { self =>
  def foreach[A, U](fa: F[A])(f: A => U): Unit

  /** Composition of foreach over nested types. */
  def compose[G[_]](implicit G: Foreach[G]): Foreach[λ[α => F[G[α]]]] = new Foreach[λ[α => F[G[α]]]] {
    override def foreach[A, U](fa: F[G[A]])(f: A => U): Unit = self.foreach(fa)(ga => G.foreach(ga)(f))
  }
}

object Foreach extends LowPriForeach {
  def apply[F[_]](implicit ev: Foreach[F]): Foreach[F] = ev

  /** Foreach evidence of [[Option]]. */
  implicit def optionForeach: Foreach[Option] = new Foreach[Option] {
    override def foreach[A, U](fa: Option[A])(f: A => U): Unit = fa.foreach(f)
  }

  /** Foreach evidence of [[GenTraversableOnce]]. */
  implicit def gt1Foreach[F[X] <: GenTraversableOnce[X]]: Foreach[F] = new Foreach[F] {
    override def foreach[A, U](fa: F[A])(f: A => U): Unit = fa.foreach(f)
  }

  /** Foreach evidence of [[Id]]. */
  implicit def idForeach: Foreach[Id] = new Foreach[Id] {
    override def foreach[A, U](fa: Id[A])(f: A => U): Unit = f(fa)
  }
}

trait LowPriForeach {

  /** Foreach evidence of a type with [[Foldable]] evidence. */
  implicit def foldableForeach[F[_]: Foldable]: Foreach[F] = new Foreach[F] {
    override def foreach[A, U](fa: F[A])(f: A => U): Unit = Foldable[F].foldLeft(fa, ()) {
      case (_, a) => f(a)
    }
  }
}
