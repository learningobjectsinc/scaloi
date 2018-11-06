package scaloi

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

object std {

  object cbf {

    /** Zero evidence for a CBF type. */
    implicit def cbfZero[CC[_] <: GenTraversableOnce[_], T](implicit cbf: CanBuildFrom[Nothing, T, CC[T]]): Zero[CC[T]] =
      new Zero[CC[T]] {
        override def zero: CC[T] = cbf().result
        override def isZero(a: CC[T]): Boolean = a.isEmpty
      }
  }

  object ju {
    import java.{util => jutil}

    /** Zero evidence for [[jutil.List]]. */
    implicit def juZero[A]: Zero[jutil.List[A]] = Zero.instance(new jutil.ArrayList[A](), _.isEmpty)
  }

}
