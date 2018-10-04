package scaloi
package syntax

import java.{lang => jl}

/** Enhancements on [[Double]]s. */
final class DoubleOps(private val self: Double) extends AnyVal {
  import DoubleOps._
  import jl.Double._

  /** Check whether this [[Double]] is within [[ε]] of `other`. */
  def ≈(other: Double): Boolean =
    longBitsToDouble(doubleToRawLongBits(self - other) & SignMasque) < ε

  /** Check whether this [[Double]] is less than `other`, within [[ε]]. */
  def ⪅(other: Double): Boolean = self < other || (this ≈ other)

  /** Check whether this [[Double]] is greater than `other`, within [[ε]]. */
  def ⪆(other: Double): Boolean = self > other || (this ≈ other)

}

object DoubleOps extends ToDoubleOps {
  final val ε                  = 0.0001d // capriciously chosen
  private final val SignMasque = 0x7FFFFFFFFFFFFFFFL
}

trait ToDoubleOps {
  import language.implicitConversions

  @inline
  implicit final def ToDoubleOps(self: Double): DoubleOps = new DoubleOps(self)

}
