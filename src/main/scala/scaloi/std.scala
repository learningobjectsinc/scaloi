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
