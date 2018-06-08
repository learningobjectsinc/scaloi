package scaloi
package syntax

import enumeratum.EnumEntry

final class EnumEntryOps[E <: EnumEntry](private val self: E) extends AnyVal {

  /** Summon the enum companion object for this enum entry.
    */
  @inline
  def enum(implicit ee: misc.Enumerative[E]): ee.enum.type = ee.enum

}

object EnumEntryOps extends ToEnumEntryOps

trait ToEnumEntryOps {
  import language.implicitConversions

  @inline
  implicit final def ToEnumEntryOps[E <: EnumEntry](self: E): EnumEntryOps[E] =
    new EnumEntryOps[E](self)

}
