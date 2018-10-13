package scaloi

import scalaz.Monoid

/** The moral ancestor of [[Monoid]]. */
trait Zero[A] {
  /** Get the zero value of the [[A]] type. */
  def zero: A
}

object Zero {
  def apply[A](implicit ev: Zero[A]): Zero[A] = ev

  def instance[A](z: A): Zero[A] = new Zero[A] { override def zero: A = z }

  def zero[A](implicit ev: Zero[A]): A = ev.zero

  /** Zero evidence for a monoidal type. */
  implicit def monoidalZero[A : Monoid]: Zero[A] = Zero instance Monoid[A].zero
}
