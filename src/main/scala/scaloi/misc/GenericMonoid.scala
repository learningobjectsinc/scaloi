/*
 * Copyright 2007 Cengage Learning, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scaloi
package misc

import scalaz.Monoid
import shapeless._
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
object GenericMonoid {

  sealed trait MkMonoid[HL <: HList] { val monoid: Monoid[HL] }
  object MkMonoid {
    /** Monoidal evidence of a cons. */
    implicit def monoidHCons[H, T <: HList](
      implicit
      hMonoid: Monoid[H],
      tMonoid: Lazy[MkMonoid[T]]
    ): MkMonoid[H :: T] = new MkMonoid[H :: T] {
      val monoid =
        new Monoid[H :: T] {
          override val zero: H :: T =
            hMonoid.zero :: tMonoid.value.monoid.zero

          override def append(a: H :: T, b: => H :: T): H :: T =
            hMonoid.append(a.head, b.head) :: tMonoid.value.monoid.append(a.tail, b.tail)
        }
    }

    /** Monoidal evidence of nil. */
    implicit val monoidHNil: MkMonoid[HNil] =
      new MkMonoid[HNil] {
        val monoid = new Monoid[HNil] {
          override val zero = HNil
          override def append(f1: HNil, f2: => HNil): HNil = HNil
        }
      }
  }

  /** Monoidal evidence of a generic type. */
  implicit def monoidGeneric[T, R <: HList](
    implicit generic: Generic.Aux[T, R],
    rMonoid: MkMonoid[R]
  ): Monoid[T] = new Monoid[T] {
    override val zero = generic.from(rMonoid.monoid.zero)
    override def append(a: T, b: => T): T = generic.from(rMonoid.monoid.append(generic.to(a), generic.to(b)))
  }

  /** A kinda-curried summoning method for generically-derived monoids.
    *
    * Use thusly:
    * {{{
    *   import scalaz.std.anyVal._
    *   import scaloi.misc._
    *   case class Ints(i: Int, j: Int)
    *   implicit val intsMonoid: Monoid[Ints] = GenericMonoid[Ints]()
    * }}}
    */
  @inline def apply[T] = new applied[T]
  final class applied[T] {
    @inline
    def apply[Repr <: HList]()(implicit gen: Generic.Aux[T, Repr], mk: MkMonoid[Repr]): Monoid[T] =
      monoidGeneric[T, Repr](gen, mk)
  }
}
