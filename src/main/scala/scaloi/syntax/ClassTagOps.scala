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

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
  * Enhancements on class tags.
  *
  * @param self the class tag instance
  * @tparam C the class type
  */
final class ClassTagOps[C](private val self: ClassTag[C]) extends AnyVal {

  /**
    * Cast a value to this class type, if it is type compatible.
    *
    * @param o the value
    * @return the value as the target type, if it is compatible
    */
  def option(o: AnyRef): Option[C] =
    if (self.runtimeClass.isInstance(o))
      Some(o.asInstanceOf[C])
    else
      None
}

trait ClassTagFns {

  /**
    * Returns the runtime class of a type with ClassTag evidence.
    *
    * @tparam T the type of interest
    * @return the runtime class
    */
  def classTagClass[T: ClassTag]: Class[T] =
    reflect.classTag[T].runtimeClass.asInstanceOf[Class[T]]
}

/**
  * Implicit conversion for class tag operations.
  */
trait ToClassTagOps {

  /**
    * Implicit conversion from class tag to the class tag enhancements.
    * @param ct the class tag
    * @tparam C its type
    */
  implicit def toClassTagOps[C](ct: ClassTag[C]): ClassTagOps[C] = new ClassTagOps(ct)
}
