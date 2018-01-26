package scaloiz.syntax

import scaloi.syntax.AnyOpsCommon

import scalaz.Monoid

/**
  * EnhancementZ on anything.
  *
  * @tparam A
  */
final class AnyOps[A](val self: A) extends AnyVal with AnyOpsCommon[A] {
  /**
    * `b` if `self` is not null, or the monoidal zero otherwise.
    *
    * @param b the computation to return if `self` is non-null
    * @param M evidence that `B` is a monoid
    * @param ev evidence that `A` might be `null`
    * @tparam B the result type
    * @return `b` if `self` is non-null; `M.zero` otherwise.
    */
  @inline final def ?|[B](b: => B)(implicit M: Monoid[B], ev: Null <:< A): B =
  if (self == ev(null)) M.zero else b

  /** Transform this value only if non-zero.
    *
    * @param transform the transformation
    * @tparam B the transformed type
    * @return this value, optionally transformed
    */
  @inline final def transformNZ[B >: A](transform: A => B)(implicit ev: Monoid[A]): B = if (self != ev.zero) transform(self) else self
}

object AnyOps extends ToAnyOps

/**
  * Implicit conversion for any operation.
  */
trait ToAnyOps {
  /**
    * Implicit conversion from anything to the any enhancements.
    * @param a the thing
    * @tparam A its type
    */
  implicit def toAnyOps[A](a: A): AnyOps[A] = new AnyOps[A](a)
}