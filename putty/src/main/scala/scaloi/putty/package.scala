package scaloi

package object putty {
  //type ClassMap[U, L <: U] = ClassMap.ClassMap[U, L]

  /**
    * A [[scaloi.ClassMap]] with no lower bound.
    * @tparam U the upper bound of the types of values in this [[scaloi.ClassMap]]
    */
  type ClassMap0[U] = ClassMap[U, Nothing]
}
