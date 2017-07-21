package scaloi.syntax

import java.util.Map.Entry

import scala.language.implicitConversions

/**
  * Map entry pimping operations.
  *
  * @param self the entry to pimp
  * @tparam A the key type
  * @tparam B the value type
  */
final class EntryOps[A, B](val self: Entry[A, B]) extends AnyVal {

  /**
    * Convert a map entry to a scala tuple.
    *
    * @return a tuple
    */
  def asScala: (A, B) = self.getKey -> self.getValue
}

/**
  * Map entry operations companion.
  */
object EntryOps extends ToEntryOps

/**
  * Implicit conversion for map entry operations.
  */
trait ToEntryOps {

  /**
    * Implicit conversion from map entry to the entry enhancements.
    * @param e the entry
    * @tparam A the key type
    * @tparam B the value type
    */
  implicit def toEntryOps[A, B](e: Entry[A, B]): EntryOps[A, B] = new EntryOps(e)
}
