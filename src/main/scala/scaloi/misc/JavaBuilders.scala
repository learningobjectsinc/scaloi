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
package misc

import java.{lang, util}
import scala.collection.{Factory, mutable}
import scala.jdk.CollectionConverters._

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
  implicit final def JavaList[A]: Factory[A, util.List[A]] =
    new Factory[A, util.List[A]] {
      override def fromSpecific(it : IterableOnce[A]) : util.List[A] = {
        val cc = new util.LinkedList[A]
        it.iterator.foreach(cc.add)
        cc
      }

      def newBuilder : mutable.Builder[A, util.List[A]] = new mutable.Builder[A, util.List[A]] {
        val cc = new util.LinkedList[A]

        override def clear(): Unit = cc.clear()

        override def result(): util.List[A] = cc

        override def addOne(a: A): this.type = {
          cc.add(a)
          this
        }
      }
    }

  /** A builder factory for `java.util.HashMap`s. */
  implicit final def JavaMap[K, V]: Factory[(K, V), util.Map[K, V]] =
    new Factory[(K, V), util.Map[K, V]] {
      override def fromSpecific(it : IterableOnce[(K, V)]) : util.Map[K, V] = {
        val cc = new util.HashMap[K, V]
        it.iterator.foreach(kv => cc.put(kv._1, kv._2))
        cc
      }

      def newBuilder : mutable.Builder[(K, V), util.Map[K, V]] = new mutable.Builder[(K, V), util.Map[K, V]] {
        val cc = new util.HashMap[K, V]

        override def clear(): Unit = cc.clear()

        override def result(): util.Map[K, V] = cc

        override def addOne(kv: (K, V)): this.type = {
          cc.put(kv._1, kv._2)
          this
        }
      }
    }

  /** A builder factory for `java.util.HashSet`s. */
  implicit final def JavaSet[A]: Factory[A, util.Set[A]] =
    new Factory[A, util.Set[A]] {
      override def fromSpecific(it : IterableOnce[A]) : util.Set[A] = {
        val cc = new util.HashSet[A]
        it.iterator.foreach(cc.add)
        cc
      }

      def newBuilder : mutable.Builder[A, util.Set[A]] = new mutable.Builder[A, util.Set[A]] {
        val cc = new util.HashSet[A]

        override def clear(): Unit = cc.clear()

        override def result(): util.Set[A] = cc

        override def addOne(a: A): this.type = {
          cc.add(a)
          this
        }
      }
    }


  import language.implicitConversions

  implicit final def ToJavaBuildingSyntax[A](ji: lang.Iterable[A]): JavaBuildingSyntax[A] =
    new JavaBuildingSyntax[A](ji)

}

final class JavaBuildingSyntax[A](private val self: lang.Iterable[A]) extends AnyVal {
  def to[CC[_]](implicit fac: Factory[A, CC[A]]): CC[A] =
    self.iterator.asScala.to(fac)
}

object JavaBuilders extends JavaBuilders
