/*
 * Copyright 2007 Learning Objects
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

import scalaz.{Equal, IsEmpty, Monoid}
import scaloi.Zero.instance

/** The unlawful ancestor of [[scalaz.Monoid]]. */
trait Zero[A] {
  /** Get the zero value of the `A` type. */
  def zero: A

  /** Test whether an `A` is zero. */
  def isZero(a: A): Boolean

  /** Test whether an `A` is non-zero. */
  final def nonZero(a: A): Boolean = !isZero(a)
}

object Zero extends ZeroInstances0 {
  /** Summon the implicit zero evidence of a type `A`. */
  def apply[A](implicit ev: Zero[A]): Zero[A] = ev

  /** Define zero evidence for a type `A`. */
  def instance[A](z: A, isZ: A => Boolean): Zero[A] = new Zero[A] {
    override def zero: A               = z
    override def isZero(a: A): Boolean = isZ(a)
  }

  /** Summon the zero value for a type `A`. */
  def zero[A](implicit ev: Zero[A]): A = ev.zero
}

trait ZeroInstances0 extends ZeroInstances1 {
  /** Zero evidence for an [[scalaz.IsEmpty]]. */
  implicit def derivedZeroF[F[_] : IsEmpty, A]: Zero[F[A]] = new Zero[F[A]] {
    override def zero: F[A] = IsEmpty[F].empty
    override def isZero(fa: F[A]): Boolean = IsEmpty[F].isEmpty(fa)
  }

  /** Zero evidence for a numeric. */
  implicit def numericZero[A : Numeric]: Zero[A] = new Zero[A] {
    override def zero: A = implicitly[Numeric[A]].zero
    override def isZero(a: A): Boolean = implicitly[Numeric[A]].equiv(a, implicitly[Numeric[A]].zero)
  }

  /** Unitary zero. */
  implicit def unitZero: Zero[Unit] = instance((), _ => true)

  /** Booleanic zero. */
  implicit def booleanZero: Zero[Boolean] = instance(false, !_)
}

trait ZeroInstances1 {
  /** Zero evidence for a monoidal and equal type. */
  implicit def derivedZero[A : Equal : Monoid]: Zero[A] = new Zero[A] {
    override def zero: A = Monoid[A].zero
    override def isZero(a: A): Boolean = Equal[A].equal(a, Monoid[A].zero)
  }
}
