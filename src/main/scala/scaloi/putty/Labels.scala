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

package scaloi.putty

import shapeless._
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record._
import simulacrum.typeclass

import scala.language.implicitConversions

/**
  * This typeclass when given a product type T, returns a list of strings containing the label
  * for each member of the product.
  */
@typeclass
trait Labels[T] {
  def labels: List[String]
}
object Labels {
  object toName extends Poly1 { implicit def keyToName[A] = at[Symbol with A](_.name) }

  /**
    * Generically derive a list of labels from T.
    * @tparam T THe product type T
    * @tparam TLG HList representing the labelled generic type of T
    * @tparam KS KeySet, type level set of keys
    * @tparam SS StringSet, the result of mapping the type level set of keys
    * @return
    */
  implicit def genericProductLabels[T, TLG <: HList, KS <: HList, SS <: HList](
      implicit lgen: LabelledGeneric.Aux[T, TLG],
      keys: Keys.Aux[TLG, KS],
      mapper: shapeless.ops.hlist.Mapper.Aux[toName.type, KS, SS],
      toTraversable: ToTraversable.Aux[SS, List, String]) = new Labels[T] {
    override def labels: List[String] = {
      Keys[lgen.Repr].apply.map(toName).toList
    }
  }
}
