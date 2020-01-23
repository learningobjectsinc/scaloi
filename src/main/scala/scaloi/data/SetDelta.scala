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
package data

/**
  * Representation of a change in the members of a set.
  * @param add the elements to add
  * @param remove the elements to remove
  * @tparam T the element type
  */
final case class SetDelta[T](add: Set[T], remove: Set[T]) {
  /** Alter `target` by this delta.
    *
    * Elements to be both added and removed are added.
    */
  def |:(target: Set[T]): Set[T] = (target &~ remove) | add

  /** Alter `target` if defined, or else alter the empty set.
    */
  def ?|:(target: Option[Set[T]]): Set[T] =
    target.fold(add)(_ |: this)
}

/** Set delta companion. */
object SetDelta {
  def apply[T](add: Set[T], remove: Set[T]): SetDelta[T] =
    new SetDelta(add, remove &~ add)

  def add[T](add: Set[T]): SetDelta[T] = SetDelta(add, Set.empty)
  def remove[T](remove: Set[T]): SetDelta[T] = SetDelta(Set.empty, remove)
  def empty[T]: SetDelta[T] = SetDelta(Set.empty, Set.empty)

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

  import argonaut._, Argonaut._ // no shapeless but whatever, and derive is broken
  implicit def encode[T: EncodeJson]: EncodeJson[SetDelta[T]] = EncodeJson { Δ =>
    Json.jObjectFields("add" := Δ.add, "remove" := Δ.remove)
  }
  implicit def decode[T: DecodeJson]: DecodeJson[SetDelta[T]] = DecodeJson { hc =>
    for {
      add    <- hc.field("add").as[Set[T]]
      remove <- hc.field("remove").as[Set[T]]
    } yield SetDelta(add, remove)
  }
}
