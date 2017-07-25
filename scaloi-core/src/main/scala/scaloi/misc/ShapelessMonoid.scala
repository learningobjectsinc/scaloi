package scaloi.misc

import shapeless._

import scalaz.Monoid

/**
  * Provides monoid evidence for generic monoidal things. For
  * example, a case class with just monoidal values.
  *
  * {{{
  * import scalaz.Monoid
  * import shapeless._
  * import scalaz.std.anyVal._
  * case class Foo(a: Int, b: Long)
  * implicit val fooMonoid = Monoid[Foo]
  * }}}
  */
object ShapelessMonoid {
  /** Monoidal evidence of a cons. */
  implicit def monoidHCons[H,T <: HList](
    implicit hMonoid: Monoid[H],
    tMonoid: Lazy[Monoid[T]]
  ): Monoid[H :: T] = new Monoid[H :: T] {
    override val zero: H :: T = hMonoid.zero :: tMonoid.value.zero
    override def append(a: H :: T, b: => H :: T): H :: T =
      hMonoid.append(a.head, b.head) :: tMonoid.value.append(a.tail, b.tail)
  }

  /** Monoidal evidence of nil. */
  implicit def monoidHNil: Monoid[HNil] = new Monoid[HNil] {
    override val zero = HNil
    override def append(f1: HNil, f2: => HNil): HNil = HNil
  }

  /** Monoidal evidence of a generic type. */
  implicit def monoidGeneric[T, R <: HList](
    implicit generic: Generic.Aux[T, R],
    rMonoid: Monoid[R]
  ): Monoid[T] = new Monoid[T] {
    override val zero = generic.from(rMonoid.zero)
    override def append(a: T, b: => T): T = generic.from(rMonoid.append(generic.to(a), generic.to(b)))
  }
}
