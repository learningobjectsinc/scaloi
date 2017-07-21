package scaloi.misc

/**
  * Representation of a change in the members of a set.
  * @param add the elements to add
  * @param remove the elements to remove
  * @tparam T the element type
  */
case class SetDelta[T](add: Set[T], remove: Set[T])

/** Set delta companion. */
object SetDelta {

  /** Delta from a set.
    *
    * @param before the set value before
    * @tparam T the element type
    */
  final class DeltaFrom[T](before: Set[T]) {
    /**
      * Compute the delta to a set.
      * @param after the set value after
      * @return the set delta
      */
    def to(after: Set[T]): SetDelta[T] = SetDelta(after -- before, before -- after)
  }

  /**
    * Get the delta from a set. Use so:
    * {{{
    *   val threeOne = SetDelta from Set(1, 2) to Set(2, 3)
    * }}}
    * @param before the set value before
    * @tparam T the element type
    * @return the delta from
    */
  def from[T](before: Set[T]): DeltaFrom[T] = new DeltaFrom(before)
}
