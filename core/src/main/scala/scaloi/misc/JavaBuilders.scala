package scaloi
package misc

import java.{util => ju}

import scala.collection.generic.CanBuildFrom
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

}

object JavaBuilders extends JavaBuilders
