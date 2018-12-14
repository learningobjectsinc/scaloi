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
package misc

import java.{lang => jl, util => ju}

import scala.collection.generic.CanBuildFrom
import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Generic implicit builders for java collection types.
  *
  * This allows you to write, for instance:
  * {{{
  *   scala> ("a" :: "b" :: "c" :: Nil).to[java.util.List]
  *   res0: java.util.List[String] = [a, b, c]
  * }}}
  */
trait JavaBuilders {

  /** A builder factory for `java.util.LinkedList`s. */
  implicit final def canBuildJavaList[Elem]: CanBuildFrom[Nothing, Elem, ju.List[Elem]] =
    new CanBuildFrom[Nothing, Elem, ju.List[Elem]] {
      def apply(from: Nothing) = apply()
      def apply() = new mutable.Builder[Elem, ju.List[Elem]] {
        val _result: ju.List[Elem] = new ju.LinkedList[Elem]

        def +=(elem: Elem) = {
          _result add elem
          this
        }

        def clear() = _result.clear()

        def result() = _result
      }
    }

  /** A builder factory for `java.util.HashMap`s. */
  implicit final def canBuildJavaMap[Key, Value]: CanBuildFrom[Nothing, (Key, Value), ju.Map[Key, Value]] =
    new CanBuildFrom[Nothing, (Key, Value), ju.Map[Key, Value]] {
      def apply(from: Nothing) = apply()
      def apply() = new mutable.Builder[(Key, Value), ju.Map[Key, Value]] {
        val _result: ju.Map[Key, Value] = new ju.HashMap[Key, Value]()

        def +=(elem: (Key, Value)) = {
          _result.put(elem._1, elem._2)
          this
        }

        def clear() = _result.clear()

        def result() = _result
      }
    }

  /** A builder factory for `java.util.HashSet`s. */
  implicit final def canBuildJavaSet[Elem]: CanBuildFrom[Nothing, Elem, ju.Set[Elem]] =
    new CanBuildFrom[Nothing, Elem, ju.Set[Elem]] {
      def apply(from: Nothing) = apply()
      def apply() = new mutable.Builder[Elem, ju.Set[Elem]] {
        val _result: ju.Set[Elem] = new ju.HashSet[Elem]()

        def +=(elem: Elem): this.type = {
          _result add elem
          this
        }

        def clear(): Unit = _result.clear()

        def result() = _result
      }
    }

  import language.implicitConversions
  implicit final def ToJavaBuildingSyntax[A](ji: jl.Iterable[A]) =
    new JavaBuildingSyntax[A](ji)

}

final class JavaBuildingSyntax[A](private val self: jl.Iterable[A]) extends AnyVal {
  def to[CC[_]](implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): CC[A] =
    self.iterator.asScala.to(cbf)
}

object JavaBuilders extends JavaBuilders
