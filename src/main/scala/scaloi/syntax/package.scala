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

import scalaz.std.OptionInstances

import scala.util.{Success, Try}

package object syntax {

  /** The partial function that takes any value to `()`.
    */
  private[syntax] val constUnit: PartialFunction[Any, Unit] = { case _ => () }

  /** A successful [[Try]] containing no meaningful value.
    */
  private[syntax] val successUnit: Try[Unit] = Success(())

  object align extends ToAlignOps
  object annotation extends ToAnnotationOps
  object any extends ToAnyOps
  object boolean extends ToBooleanOps
  object box extends ToBoxOps
  object `class` extends ToClassOps
  object classTag extends ToClassTagOps with ClassTagFns
  object cobind extends ToCobindOps
  object boxes extends ToCollectionBoxOps with OptionInstances
  object collection extends ToCollectionOps
  object date extends ToDateOps with DateInstances
  object ⋁ extends ToDisjunctionOps with DisjunctionFns
  object disjunction extends ToDisjunctionOps with DisjunctionFns
  object double extends ToDoubleOps with DoubleVals
  object either extends ToEitherOps
  object entry extends ToEntryOps
  object enumeratum extends ToEnumEntryOps
  object finiteDuration extends ToFiniteDurationOps
  object foldable extends ToFoldableOps
  object functor extends ToFunctorOps
  object hypermonad extends ToHypermonadOps
  object instant extends ToInstantOps
  object enum extends ToJEnumOps
  object listTree extends ToListTreeOps
  object lock extends ToLockOps
  object map extends ToMapOps
  object monad extends ToMonadOps with ToFunctorOps
  object monadPlus extends ToMonadPlusOps with ToMonadOps with ToFunctorOps
  object monoid extends ToMonoidOps
  object mutableMap extends ToMutableMapOps
  object option extends ToOptionOps
  object partialFunction extends ToPartialFunctionOps
  object =∂> extends ToPartialFunctionOps
  object readWriteLock extends ToReadWriteLockOps
  object regex extends ToRegexOps
  object seq extends ToSeqOps
  object set extends ToSetOps
  object string extends ToStringOps
  object strictTree extends ToStrictTreeOps
  object task extends ToTaskOps
  object tree extends ToTreeOps
  object `try` extends ToTryOps with ToTryAnyOps with ToTryCompanionOps
  object validation extends ToValidationOps
  object zero extends ToZeroOps
}
