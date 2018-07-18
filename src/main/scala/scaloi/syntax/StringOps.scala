package scaloi
package syntax

import scala.util.Try

/** Enhancements on strings.
  */
final class StringOps(private val self: String) extends AnyVal {

  def toBoolean_! : Try[Boolean] = Try(self.toBoolean)
  def toBoolean_? : Option[Boolean] = this.toBoolean_!.toOption

  def toLong_! : Try[Long] = Try(self.toLong)
  def toLong_? : Option[Long] = this.toLong_!.toOption

}

object StringOps extends ToStringOps

trait ToStringOps {
  import language.implicitConversions

  @inline implicit final def toStringOps(self: String): StringOps =
    new StringOps(self)
}
