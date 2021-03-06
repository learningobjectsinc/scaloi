/*
 * Copyright 2007 Learning Objects
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

import scala.collection.Factory

object std {

  object collection {

    /** Zero evidence for a collection type. */
    implicit def collectionZero[CC[_] <: IterableOnce[_], T](implicit fac: Factory[T, CC[T]]): Zero[CC[T]] =
      new Zero[CC[T]] {
        override def zero: CC[T] = fac.newBuilder.result()
        override def isZero(a: CC[T]): Boolean = a.iterator.isEmpty
      }
  }

  object ju {
    import java.{util => jutil}

    /** Zero evidence for [[java.util.List]]. */
    implicit def juZero[A]: Zero[jutil.List[A]] = Zero.instance(new jutil.ArrayList[A](), _.isEmpty)
  }

}
