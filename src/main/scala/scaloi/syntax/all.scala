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
package syntax

/** All the syntacies. */
trait all extends AnyRef
  with ToAlignOps
  with ToAnnotationOps
  with ToAnyOps
  with ToBooleanOps
  with ToBoxOps
  with ToClassOps
  with ToClassTagOps
  with ToCobindOps
  with ToCollectionBoxOps
  with ToCollectionOps
  with ToDateOps
  with ToDisjunctionOps
  with ToDoubleOps
  with ToEitherOps
  with ToEnumEntryOps
  with ToEntryOps
  with ToFiniteDurationOps
  with ToFoldableOps
  with ToFunctorOps
  with ToHypermonadOps
  with ToInstantOps
  with ToJEnumOps
  with ToLockOps
  with ToMapOps
  with ToMonadOps
  with ToMonadPlusOps
  with ToMonoidOps
  with ToMutableMapOps
  with ToOptionOps
  with ToPartialFunctionOps
  with ToReadWriteLockOps
  with ToRegexOps
  with ToSeqOps
  with ToSetOps
  with ToStringOps
  with ToStrictTreeOps
  with ToTaskOps
  with ToTreeOps
  with ToTryOps
  with ToTryAnyOps
  with ToTryCompanionOps
  with ToValidationOps
  with ToZeroOps

object all extends all
