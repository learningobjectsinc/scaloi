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
