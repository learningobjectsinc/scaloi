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

package scaloi.misc

import scalaz.{Order, Semigroup}

/**
  * Miscellaneous semigroups. Scalaz defines similar semigroups but only for tagged types.
  */
object Semigroups {

  /** The max semigroup for an untagged ordered type.
    *
    * @tparam A the type with order evidence
    * @return semigroup evidence
    */
  implicit def maxSemigroup[A : Order]: Semigroup[A] = Semigroup.instance((a, b) => Order[A].max(a, b))


  /** The min semigroup for an untagged ordered type.
    *
    * @tparam A the type with order evidence
    * @return semigroup evidence
    */
  implicit def minSemigroup[A : Order]: Semigroup[A] = Semigroup.instance((a, b) => Order[A].min(a, b))


  /**
    * Semigroup evidence for throwables, prefers the first of a series of exceptions.
    */
  implicit def throwableSemiGroup[A <: Throwable] = Semigroup.firstSemigroup[A]
}
