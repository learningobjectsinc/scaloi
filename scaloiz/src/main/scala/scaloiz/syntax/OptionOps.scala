package scaloiz
package syntax

import scalaz._
import scalaz.std.option._
import scalaz.syntax.std.option._
import scaloi.syntax.{ToOptionOps => CoreOptionOps}

final class OptionOps[A](val self: Option[A]) extends AnyVal with ToOptionOps {
  /**
    * Flat to left disjunction. Turns this option into a left disjunction if present,
    * or else returns the supplied disjunction.
    * @param f the disjunction if this option is absent
    * @tparam AA the left type
    * @tparam B the right type
    * @return the resulting disjunction
    */
  @inline def <\/-[AA >: A, B](f: => A \/ B) =
  self.toLeftDisjunction(()).flatMap(_ => f)

  /**
    * Returns this, if present, or else optionally the supplied value if non-zero.
    * @param b the value
    * @tparam B the value type, with monoid evidence.
    * return this or else that
    */
  @inline def orNZ[B >: A : Monoid](b: => B): Option[B] = self orElse OptionOps.OptionNZ(b)

  /**
    * Filter the value to be non-zero.
    * @param ev monoid evidence for A
    * return this if non-zero
    */
  @inline def filterNZ(implicit ev: Monoid[A]): Option[A] = self - ev.zero

  /**
    * Runs the provided function as a side-effect if this is `None`, returns this option.
    * @param action the thing to do if this option is none
    * @return this option
    */
  def -<|[U](action: => U): self.type = {
    self ifNone { action ; () }
    self
  }


  /**
    * Put `self` on the left, and `right` on the right, of an Eitherneitherboth.
    *
    * @param right the option to put on the right
    * @return an Eitherneitherboth with `self` on the left and `right` on the right
    */
  @inline def \|/[B](right: Option[B]): A \|/ B =
  scaloiz.\|/(self, right)


  /**
    * Append this optional value with another value in a semigroup.
    * @param b the other value
    * @tparam B the other type, with semigroup evidence
    * @return either the other value or the combined values
    */
  private[this] def append[B >: A : Semigroup](b: B): B = self.fold(b)(Semigroup[B].append(_, b))

  /**
    * Get the maximum of two optional values.
    * @param b the other optional value
    * @tparam B the other type
    * @return the max of the optional values
    */
  @inline def max[B >: A : Order](b: Option[B]): Option[B] = maxMonoid.append(self, b)


  /**
    * Get the maximum of this and a value.
    * @param b the other value
    * @tparam B the other type
    * @return the max of the values
    */
  @inline def max[B >: A : Order](b: B): B = append(b)(misc.Semigroups.maxSemigroup)

  /**
    * Get the minimum of two optional values.
    * @param b the other optional value
    * @tparam B the other type
    * @return the min of the optional values
    */
  @inline def min[B >: A : Order](b: Option[B]): Option[B] = minMonoid.append(self, b)

  /**
    * Get the minimum of this and a value.
    * @param b the other value
    * @tparam B the other type
    * @return the min of the values
    */
  @inline def min[B >: A : Order](b: B): B = append(b)(misc.Semigroups.minSemigroup)

  /**
    * Wrap the contained value in a `Gotten`, or create one with the
    * provided thunk and wrap it in a `Created.`
    * @param b the computation to create a value
    * @tparam B the type of the created value
    * @return the contained gotten value, or the supplied created value
    */
  @inline def orCreate[B >: A](b: => B): GetOrCreate[B] = self match {
    case Some(gotten) => GetOrCreate.gotten(gotten)
    case None         => GetOrCreate.created(b)
  }
}

object OptionOps extends ToOptionOps

trait ToOptionOps extends Any with CoreOptionOps {
  /**
    * Implicit conversion from option to the option enhancements.
    * @param o the optional thing
    * @tparam A its type
    */
  implicit def toOptionOpsZ[A](o: Option[A]): OptionOps[A] = new OptionOps(o)


  /** Returns some if a value is non-null and non-zero, or else none.
    * @param a the value
    * @tparam A the value type with monoid evidence
    * @return the option
    */
  def OptionNZ[A : Monoid](a: A): Option[A] = Option(a).filterNZ

  /** Monoid evidence for the minimum over an option of an ordered type. */
  def minMonoid[A : Order]: Monoid[Option[A]] = optionMonoid(misc.Semigroups.minSemigroup)

  /** Monoid evidence for the maximum over an option of an ordered type. */
  def maxMonoid[A : Order]: Monoid[Option[A]] = optionMonoid(misc.Semigroups.maxSemigroup)
}