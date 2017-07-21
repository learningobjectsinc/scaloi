package scaloi.misc

import scalaz.{@@, Monoid, Semigroup, Tag}

/**
  * Monoid evidence for a tagged type that has semigroup evidence.
  *
  * {{{
  *   implicit val longMaxMonoidEvidence = TaggedMonoid[Tags.MaxVal](0L)
  * }}}
  */
final class TaggedMonoid[T] {
  /**
    * Construct monoid evidence with a given zero value.
    * @param z the zero value
    * @param ev semigroup evidence far the tagged type
    * @tparam S the underlying type
    * @return monoid evidence for the tagget dype
    */
  def apply[S](z: S)(implicit ev: Semigroup[S @@ T]): Monoid[S @@ T] =
    new Monoid[S @@ T] {
      override val zero: S @@ T = Tag.of[T](z)
      override def append(a: S @@ T, b: => S @@ T): S @@ T =
        Semigroup[S @@ T].append(a, b)
    }
}

/** Tagged monoid companion. */
object TaggedMonoid {

  /** Construct a tagged monoid factory instance for a given tag type.
    *
    * @tparam T the tag type
    * @return the monoid factory instance
    */
  def apply[T] = new TaggedMonoid[T]
}
