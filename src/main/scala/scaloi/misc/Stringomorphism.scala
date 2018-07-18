package scaloi
package misc

import java.time.Instant

import enumeratum.EnumEntry
import scaloi.syntax.StringOps._

import scala.util.{Success, Try}

/** A stringomorphism represents a transformation from a string
  * to a type, or an error if the transformation falise.
  * @tparam T the target type
  */
trait Stringomorphism[T] extends (String => Try[T])

object Stringomorphism {
  def apply[T](implicit st: Stringomorphism[T]): Stringomorphism[T] = st

  /** A homomomorphism for strings. */
  implicit val Stringhomorphism: Stringomorphism[String] = s => Success(s)

  /** A stringomorphism to an [[Instant]]. */
  implicit val InstantStringomorphism: Stringomorphism[Instant] = s => Try(Instant.parse(s))

  /** A stringomorphism to a [[Boolean]]. */
  implicit val BooleanStringomorphism: Stringomorphism[Boolean] = s => Try(s.toBoolean)

  /** A stringomorphism to a [[Long]]. */
  implicit val LongStringomorphism: Stringomorphism[Long] = _.toLong_!

  /** A stringomorphism to an enumeratum [[EnumEntry]]. */
  implicit def enumeratumStringomorphism[E <: EnumEntry: Enumerative]: Stringomorphism[E] =
    s => Try(Enumerative[E].enum.withName(s))
}
