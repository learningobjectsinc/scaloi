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

package scaloi.syntax

import java.{lang => jl}

import scaloi.misc.{Boxes, JavaBuilders}
import scaloi.misc.JavaOptionalInstances

import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalaz.Functor
import scalaz.std.OptionInstances

/** A collection of extension methods for dealing with collections of numeric types. */
object CollectionBoxOps extends ToCollectionBoxOps with OptionInstances

/** @tparam Coll the containing collection type
  * @tparam Elem the boxed object type
  */
final class BoxedCollectionOps[Coll[T], Elem](val self: Coll[Elem]) extends AnyVal {

  /**
    * Convert the members of a collection from boxed to unboxed representation.
    *
    * Use as:
    * Seq[jl.Long](new jl.Long(1L)).unboxInside() : Seq[Long] = Seq(1L)
    *
    */
  def unboxInside: CollectionUnboxer[Coll, Elem, Coll] =
    new CollectionUnboxer[Coll, Elem, Coll](self)

  /**
    * Convert the members of a collection from boxed to unboxed representation,
    * while also changing the collection type.
    *
    * Use as:
    * Seq[jl.Long](new jl.Long(1L)).unboxInsideTo[List]() : List[Long] = List(1L)
    *
    */
  def unboxInsideTo[Out[_]]: CollectionUnboxer[Coll, Elem, Out] =
    new CollectionUnboxer[Coll, Elem, Out](self)
}

final class CollectionUnboxer[Coll[T], Elem, Out[_]](val self: Coll[Elem]) extends AnyVal {
  def apply[UnboxedElem <: AnyVal]()(
      implicit boxes: Boxes[UnboxedElem, Elem],
      ap: CBOApplicable[Coll, Out, Elem, UnboxedElem]
  ): Out[UnboxedElem] =
    ap.map(self)(boxes.unbox)
}

/** @tparam Coll the containing collection type
  * @tparam Elem the unboxed primitive type
  */
final class UnboxedCollectionOps[Coll[T], Elem <: AnyVal](val self: Coll[Elem]) extends AnyVal {

  /**
    * Convert the members of a collection from unboxed to boxed representation.
    *
    * Use as:
    * Seq[Long](1L).boxInside() : Seq[jl.Long] = Seq(1L)
    *
    */
  def boxInside: CollectionBoxer[Coll, Elem, Coll] =
    new CollectionBoxer[Coll, Elem, Coll](self)

  /**
    * Convert the members of a collection from unboxed to boxed representation,
    * while also changing the collection type.
    *
    * Use as:
    * Seq[Long](1L).boxInsideTo[List]() : List[jl.Long] = List(1L)
    *
    */
  def boxInsideTo[Out[_]]: CollectionBoxer[Coll, Elem, Out] =
    new CollectionBoxer[Coll, Elem, Out](self)
}

final class CollectionBoxer[Coll[T], Elem <: AnyVal, Out[_]](val self: Coll[Elem]) extends AnyVal {
  def apply[BoxedElem]()(
      implicit boxes: Boxes[Elem, BoxedElem],
      ap: CBOApplicable[Coll, Out, Elem, BoxedElem]
  ): Out[BoxedElem] =
    ap.map(self)(boxes.box)
}

trait ToCollectionBoxOps extends JavaBuilders with JavaOptionalInstances {

  import language.implicitConversions

  implicit def toBoxedCollectionOps[Elem, CollIn[T]](self: CollIn[Elem]): BoxedCollectionOps[CollIn, Elem] =
    new BoxedCollectionOps[CollIn, Elem](self)

  implicit def toUnboxedCollectionOps[Elem <: AnyVal, CollIn[T]](
      self: CollIn[Elem]): UnboxedCollectionOps[CollIn, Elem] =
    new UnboxedCollectionOps[CollIn, Elem](self)
}

trait CBOApplicable[CollIn[_], CollOut[_], ElemIn, ElemOut] {
  def map(ci: CollIn[ElemIn])(f: ElemIn => ElemOut): CollOut[ElemOut]
}

//noinspection ConvertExpressionToSAM
object CBOApplicable extends CBOApplicable0 {
  implicit def apFunctor[F[_], ElemIn, ElemOut](implicit F: Functor[F]): CBOApplicable[F, F, ElemIn, ElemOut] =
    new CBOApplicable[F, F, ElemIn, ElemOut] {
      def map(ci: F[ElemIn])(f: (ElemIn) => ElemOut): F[ElemOut] =
        F.map(ci)(f)
    }

  implicit def mkApGenTrav[CollIn[_], CollOut[_], ElemIn, ElemOut](
      implicit T: CollIn[ElemIn] <:< GenTraversable[ElemIn],
      cbf: CanBuildFrom[Nothing, ElemOut, CollOut[ElemOut]]
  ): CBOApplicable[CollIn, CollOut, ElemIn, ElemOut] =
    new CBOApplicable[CollIn, CollOut, ElemIn, ElemOut] {
      def map(ci: CollIn[ElemIn])(f: (ElemIn) => ElemOut): CollOut[ElemOut] =
        ci.map[ElemOut, CollOut[ElemOut]](f)(collection.breakOut(cbf))
    }

  implicit def mkApJIterable[CollIn[_], CollOut[_], ElemIn, ElemOut](
      implicit T: CollIn[ElemIn] <:< jl.Iterable[ElemIn],
      cbf: CanBuildFrom[Nothing, ElemOut, CollOut[ElemOut]]
  ): CBOApplicable[CollIn, CollOut, ElemIn, ElemOut] =
    new CBOApplicable[CollIn, CollOut, ElemIn, ElemOut] {
      import collection.JavaConverters._
      def map(ci: CollIn[ElemIn])(f: (ElemIn) => ElemOut): CollOut[ElemOut] =
        ci.iterator.asScala.map(f).to[CollOut](cbf)
    }
}

sealed abstract class CBOApplicable0 {

  implicit def mkApOptional2Alternative[OptIn[_], AltOut[_], ElemIn, ElemOut](
      implicit Opt: scalaz.Optional[OptIn],
      Alt: scalaz.Alternative[AltOut]
  ): CBOApplicable[OptIn, AltOut, ElemIn, ElemOut] =
    new CBOApplicable[OptIn, AltOut, ElemIn, ElemOut] {
      def map(ci: OptIn[ElemIn])(f: (ElemIn) => ElemOut): AltOut[ElemOut] =
        Opt.pextract(ci).fold((_: OptIn[Nothing]) => Alt.empty[ElemOut], ei => Alt.point(f(ei)))
    }
}
