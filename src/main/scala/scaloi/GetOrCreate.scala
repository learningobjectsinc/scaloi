package scaloi

import scalaz.{Functor, \/}
import scalaz.syntax.std.boolean._

/**
  * Wrapper around the result of a get-or-create operation
  * which captures whether the object was just created.
  */
sealed abstract class GetOrCreate[+T] extends Product with Serializable {

  /**
    * Return the contained object.
    */
  def get: T

  /**
    * Return whether the object was created.
    */
  def isCreated: Boolean

  /**
    * Return whether the object was gotten.
    */
  def isGotten: Boolean = !isCreated

  /**
    * Map the value.
    */
  def map[U](f: T => U): GetOrCreate[U] = GetOrCreate(f(get), isCreated)

  /**
    * Run a side-effecting function on the value.
    */
  def foreach(f: T => Unit): Unit = f(get)

  /**
    * Fold the value according to whether it was created or gotten.
    * @param cf the function to apply if created
    * @param gf the function to apply if gotten
    * @tparam U the result type
    * @return the result
    */
  def fold[U](cf: T => U)(gf: T => U): U = if (isCreated) cf(get) else gf(get)

  /**
    * Return the value as a right, if this was created, or else the supplied left.
    * @param left the left value
    * @tparam U the left type
    * @return the disjunction
    */
  def createdOr[U](left: => U): U \/ T = isCreated.either(get).or(left)

  /**
    * Runs a side-effecting function on the value of this get-or-create
    * if it was created, then returns this get-or-create.
    * @param f the side-effecting function
    * @tparam A the result type, discarded
    * @return this get-or-create
    */
  def init[A](f: T => A): GetOrCreate[T] = { if (isCreated) f(get); this }

  /**
    * Runs a side-effecting function on the value of this get-or-create
    * if it was gotten, then returns this get-or-create.
    * @param f the side-effecting function
    * @tparam A the result type, discarded
    * @return this get-or-create
    */
  def update[A](f: T => A): GetOrCreate[T] = { if (isGotten) f(get); this }

  /**
    * Kestrel combinator on the value of a get-or-create.
    * @param f the side-effecting function
    * @tparam B the result type
    * @return the value
    */
  @inline final def always[B](f: T => B): T = { f(get); get }

  /**
    *  An alias for always.
    */
  @inline final def *<|[B](f: T => B): T = always(f)
}

case class Gotten[+T](value: T) extends GetOrCreate[T] {
  override def get: T = value
  override def isCreated = false
}

case class Created[+T](value: T) extends GetOrCreate[T] {
  override def get: T = value
  override def isCreated = true
}

object GetOrCreate {
  def apply[T](t: T, created: Boolean): GetOrCreate[T] =
    if (created) Created(t) else Gotten(t)

  /**
    * Factory method for objects that were got and not created.
    */
  def gotten[T](t: T): GetOrCreate[T] = Gotten(t)

  /**
    * Factory method for objects that were created.
    */
  def created[T](t: T): GetOrCreate[T] = Created(t)

  implicit val getOrCreateInstance: Functor[GetOrCreate] =
    new Functor[GetOrCreate] {
      def map[A, B](fa: GetOrCreate[A])(f: A => B) = fa map f
    }
}
