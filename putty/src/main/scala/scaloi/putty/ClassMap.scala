package scaloi.putty

import shapeless._

object ClassMap {

  /**
    * A map from classes to values of those classes' types.
    * @tparam U the upper bound on the class types
    * @tparam L the lower bound on the class types
    */
  type ClassMap[U, L <: U] = HMap[Bounds[U, L]#λ]

  /**
    * Create an empty `ClassMap` with the given bounds.
    * @tparam U the upper bound on the class types
    * @tparam L the lower bound on the class types
    */
  def empty[U, L <: U]: ClassMap[U, L] = HMap.empty[Bounds[U, L]#λ]

  /**
    * Create an empty `ClassMap` with the given upper bound and no lower bound.
    * @tparam U the upper bound on the class types
    */
  def empty0[U]: ClassMap[U, Nothing] = empty[U, Nothing]

  // === INTERNAL ===

  sealed trait Bounds[U, L <: U] {
    /* can't give a message containing `U` and `V`... sadness */
    @annotation.implicitNotFound(
      """Cannot add a key of type `${K}` and a value of type `${V}` to a `ClassMap`. Ensure that `${K}` is `Class[${V}]`, and `${V}` conforms to its bounds."""
    )
    sealed trait λ[K, V]
  }

  object Bounds {
    private[this] val _prefix: Bounds[Any, Nothing] = new Bounds[Any, Nothing] {}
    private[this] val _lambda: _prefix.λ[Any, Any] = new _prefix.λ[Any, Any] {}

    implicit def impl[U, L <: U, A >: L <: U]: Bounds[U, L]#λ[Class[A], A] = {
      val prefix: Bounds[U, L] = _prefix.asInstanceOf[Bounds[U, L]]
      _lambda.asInstanceOf[prefix.λ[Class[A], A]]
    }
  }


}
