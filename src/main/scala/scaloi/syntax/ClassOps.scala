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
package syntax

import java.lang.annotation.Annotation

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}
import scalaz.syntax.std.boolean._

/**
  * Enhancements on classes.
  *
  * @param self the class instance
  * @tparam C the class type
  */
final class ClassOps[C](private val self: Class[C]) extends AnyVal {
  import classTag.classTagClass

  /**
    * Get an annotation on this class.
    *
    * @tparam T the annotation type
    * @return the annotation, if present
    */
  def annotation[T <: Annotation: ClassTag]: Option[T] =
    Option(self.getAnnotation(classTagClass[T]))

  /**
    * Cast a value to this class type, if it is type compatible.
    *
    * @param o the value
    * @return the value as the target type, if it is compatible
    */
  def option(o: AnyRef): Option[C] =
    self.isInstance(o) option self.cast(o)

  /** Convert this Java reflection class object into a Scala reflection symbol.
    */
  def asScala: ru.ClassSymbol =
    mirror.classSymbol(self)

  /** Produce a reflection mirror which could load this class.
    */
  def mirror: ru.Mirror =
    ru.runtimeMirror(self.getClassLoader)
}

/**
  * Implicit conversion for class tag operations.
  */
trait ToClassOps {

  /**
    * Implicit conversion from class to the class enhancements.
    * @param c the class
    * @tparam C its type
    */
  implicit def toClassOps[C](c: Class[C]): ClassOps[C] = new ClassOps(c)
}
