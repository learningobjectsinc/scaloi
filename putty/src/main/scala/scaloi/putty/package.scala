package scaloi

package object putty {
  type ClassMap[U, L <: U] = ClassMap.ClassMap[U, L]

  type ClassMap0[U] = ClassMap[U, Nothing]
}
