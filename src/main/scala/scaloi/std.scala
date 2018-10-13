package scaloi

import scala.collection.generic.CanBuildFrom

object std {

  object cbf {

    /** Zero evidence for a CBF type. */
    implicit def cbfZero[CC[_], T](implicit cbf: CanBuildFrom[Nothing, T, CC[T]]): Zero[CC[T]] =
      new Zero[CC[T]] {
        override def zero: CC[T] = cbf().result
      }
  }

}
