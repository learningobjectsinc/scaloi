package scaloi
package syntax

import java.util.Optional

import scalaz.{Monoid, Order, Semigroup, \/}
import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.std.{OptionOps => OptionOpz}

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
  * Enhancements on options.
  * @param self the option value
  * @tparam A the option type
  */
final class OptionOps[A](val self: Option[A]) extends AnyVal {
  import AnyOps._
  import OptionOps.{ minMonoid, maxMonoid }

  /**
    * Flatmap over a function to a nullable value.
    * @param f the mapping function
    * @tparam B the result type
    * @return the resulting option
    */
  @inline def flatOpt[B >: Null](f: A => B): Option[B] =
    self.flatMap(a => Option(f(a)))

  /**
    * Convert an option into a successful future, if present, else a supplied failure.
    * @param e the exception if this option is absent
    * @return an available future
    */
  @inline def toFuture(e: => Exception): Future[A] =
    self.fold(Future.failed[A](e))(Future.successful)

  /**
    * Return this option, if it does not contain the specified value, else None.
    * @param a the value to remove
    * @return this option without the specified value
    */
  @inline def -(a: A): Option[A] = self.filter(_ != a)

  /**
    * Kestrel combinator on the value of an option.
    * @param f the side-effecting function
    * @tparam B the result type
    * @return this option
    */
  @inline def tap[B](f: A => B): Option[A] = self <| { _ foreach f }

  /**
    *  An alias for tap.
    */
  @inline def <|?[B](f: A => B): Option[A] = tap(f)

  /**
    * Map this value and return the result or the monoidal zero of the target type.
    * @param f the transform
    * @tparam B the result type
    * @return the mapped value or zero
    */
  @inline def foldZ[B : Monoid](f: A => B): B = self.fold(Monoid[B].zero)(f)

  /**
    * A successful [[scala.util.Try]] of this option if present, or the given failure if empty.
    * @param failure the [[scala.util.Try]] failure if this option is empty
    * @return this option as a [[scala.util.Try]]
    */
  def toTry(failure: => Throwable): Try[A] =
    self.fold[Try[A]](Failure(failure))(Success(_))

  /**
    * An alias for [[toTry]].
    */
  def elseFailure(failure: => Throwable): Try[A] = toTry(failure)

  /**
    * An alias for [[toTry]].
    */
  @inline def <@~*(failure: => Throwable): Try[A] = toTry(failure)

  /**
    * The [[Try]] contained in this option, or a [[Failure]] wrapping the given throwable.
    * @param failure the [[scala.util.Try]] failure if this option is empty
    * @return this option as a [[scala.util.Try]]
    */
  def flatToTry[B](failure: => Throwable)(implicit ev: A <:< Try[B]): Try[B] =
    self.cata(ev, Failure(failure))

  /**
    * Transforms `target` with the contained function if it exists,
    * otherwise, returns `target`.
    *
    * @param target the value to be potentially transformed
    * @return target transformed by the contained function, if it exists, otherwise target.
    */
  def transforming[T](target: T)(implicit ev: A <:< (T => T)): T =
    self.fold(target)(f => ev(f)(target))

  /**
    * An alias for `transforming`.
    */
  @inline def ~?>[T](target: T)(implicit ev: A <:< (T => T)): T =
    this transforming target

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

  /** Turns the contents of this option into a [[Failure]] if present,
    * or succeeds with no value if absent.
    *
    * @param f a function to turn this value into an error
    * @return the resulting [[Try]]
    */
  @inline def asFailure(f: A => Throwable): Try[Unit] =
    self.cata(a => Failure(f(a)), successUnit)

  /** Turns the [[Throwable]] in this option into a [[Failure]] if present,
    * or succeeds with no value if absent.
    *
    * @return the resulting [[Try]]
    */
  @inline def asFailure(implicit ev: A <:< Throwable): Try[Unit] =
    asFailure(ev: A => Throwable)

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
  @inline def filterNZ(implicit ev: Monoid[A]): Option[A] = this - ev.zero

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
   scaloi.\|/(self, right)


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

  /**
    * Filter this option by a boolean condition being true.
    * @param b the condition
    * @return this, if `b`, else `None`
    */
  @inline def when(b: Boolean): Option[A] = if (b) self else None

  /**
    * Filter this option by a boolean condition being false.
    * @param b the condition
    * @return this, if not `b`, else `None`
    */
  @inline def unless(b: Boolean): Option[A] = if (b) None else self

  /**
    * Accepts the contents of this option if of the specified runtime type.
    * @tparam B the target type
    * @return this option if it contains the target type, or else none
    */
  @inline def accept[B: ClassTag]: Option[B] = self.flatMap(implicitly[ClassTag[B]].unapply)
}

/**
  * Option operations companion.
  */
object OptionOps extends ToOptionOps

/**
  * Implicit conversion for option operations.
  */
trait ToOptionOps extends Any {

  /**
    * Implicit conversion from option to the option enhancements.
    * @param o the optional thing
    * @tparam A its type
    */
  implicit def toOptionOps[A](o: Option[A]): OptionOps[A] = new OptionOps(o)


  /**
    * Implicit conversion from Java optional to the option enhancements.
    * @param o the optional thing
    * @tparam A its type
    */
  implicit def toOptionalOps[A >: Null](o: Optional[A]): OptionOps[A] = new OptionOps(Option(o.orElse(null)))

  /**
    * Implicit conversion from Java optional to the option enhancements.
    * @param o the optional thing
    * @tparam A its type
    */
  implicit def toOptionalOpz[A >: Null](o: Optional[A]): OptionOpz[A] = new OptionOpz(Option(o.orElse(null)))

  /** Returns some if a value is non-null and non-zero, or else none.
    * @param a the value
    * @tparam A the value type with monoid evidence
    * @return the option
    */
  def OptionNZ[A : Monoid](a: A): Option[A] = Option(a).filterNZ

  /** Returns `Some` if a value is non-null, or else `None`.
    *
    * This differs from [[Option.apply]] in that the type argument of the
    * returned `Option` changes from the boxed argument type [[A]] to the
    * unboxed type [[U]], reflecting the newly-proved nonnullability of
    * the contained value.
    */
  def Boxtion[A >: Null, U <: AnyVal](a: A)(implicit A: misc.Boxes[U, A]): Option[U] =
    Option(a).map(A.unbox)

  /** Monoid evidence for the minimum over an option of an ordered type. */
  def minMonoid[A : Order]: Monoid[Option[A]] = optionMonoid(misc.Semigroups.minSemigroup)

  /** Monoid evidence for the maximum over an option of an ordered type. */
  def maxMonoid[A : Order]: Monoid[Option[A]] = optionMonoid(misc.Semigroups.maxSemigroup)

  /** Run the given partial function, or `None` if it does not match.
    */
  def flondOpt[A, B](a: A)(f: PartialFunction[A, Option[B]]): Option[B] =
    f.applyOrElse(a, (_: A) => None)

}
