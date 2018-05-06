package scaloi
package syntax

import scalaz.{Functor, Monoid, \/}
import scalaz.syntax.std.boolean._
import scala.language.implicitConversions

/**
  * Enhancements on anything.
  *
  * @param self the value
  * @tparam A the type of A
  */
final class AnyOps[A](val self: A) extends AnyVal with AnyOpsCommon[A]

trait AnyOpsCommon[A]  extends Any {

  def self: A

  /**
    * Kestrel combinator applies a side-effect to a value and then returns
    * the value.
    * @param f the side-effect
    * @tparam B the side-effect result type
    * @return the original value
    */
  @inline final def tap[B](f: A => B): A = { f(self) ; self }

  /**
    * Kestrel combinator; an alias for tap.
    */
  @inline final def <|[B](f: A => B): A = tap(f)

  /**
    * Thrush combinator applies a value to a function. This shamelessly
    * duplicates scalaz to make imports cleaner.
    * @param f the function
    * @tparam B the result type
    * @return the result
    */
  @inline final def |>[B](f: A => B): B = f(self)

  /**
    * Kestrel combinator supporting a partial function.
    * @param f a partial function over the parameter
    * @tparam B the result type of the partial function
    * @return the original value
    */
  @inline final def pfTap[B](f: PartialFunction[A, B]): A = tap { f orElse constUnit }



  /** Transform this value only if a predicate holds true.
    *
    * @param pred the predicate
    * @param transform the transformation
    * @tparam B the transformed type
    * @return this value, optionally transformed
    */
  @inline final def transformWhen[B >: A](pred: A => Boolean)(transform: A => B): B = if (pred(self)) transform(self) else self

  /** Transform this value only if a predicate holds false.
    *
    * @param pred the predicate
    * @param transform the transformation
    * @tparam B the transformed type
    * @return this value, optionally transformed
    */
  @inline final def transformUnless[B >: A](pred: A => Boolean)(transform: A => B): B = if (pred(self)) self else transform(self)

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

  /**
    * Return this value as a right if a predicate is true, or else the supplied left.
    * @param pred the predicate
    * @param b the left value
    * @tparam B the left type
    * @return the resulting disjunction
    */
  def rightWhen[B](pred: A => Boolean)(b: => B): B \/ A   = pred(self) either self or b

  /**
    * Return this value as a right if a predicate is false, or else the supplied left.
    * @param pred the predicate
    * @param b the left value
    * @tparam B the left type
    * @return the resulting disjunction
    */
  def rightUnless[B](pred: A => Boolean)(b: => B): B \/ A = !pred(self) either self or b

  /**
    * Associate this value with the contents of a functor type.
    * @param bs the functor values
    * @tparam B the content type
    * @tparam C the functor type
    * @return the associated values
    */
  def -*>[B, C[?] : Functor](bs: C[B]): C[(A, B)] = Functor[C].map(bs)(b => self -> b)
}

/**
  * Any operations companion.
  */
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
  implicit def toAnyOps[A](a: A): AnyOps[A] = new AnyOps(a)
}
