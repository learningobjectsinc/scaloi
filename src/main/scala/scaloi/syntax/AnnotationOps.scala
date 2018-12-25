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
import java.lang.reflect.Proxy

import scala.language.implicitConversions

/**
  * Enhancements on annotation values.
  *
  * @param self the annotationvalue
  * @tparam A the annotation type
  */
final class AnnotationOps[A <: Annotation](private val self: A) extends AnyVal {

  /**
    * Override attributes of an annotation value with those specified in a
    * map.
    *
    * This can almost be achieved typely by using a scala proxy macro (e.g.
    * autoproxy or scala-macro-aop) as {{class OverideFoo(val itemType: String,
    * @@delegate proxy: Foo) extends Foo}} but ultimately scalac fails to generate
    * valid bytecode for the synthetic annotation class.
    *
    * @param attributes the attributes to override on the annotation
    * @return an annotation proxy
    */
  @inline final def ++(attributes: Map[String, AnyRef]): A =
    Proxy
      .newProxyInstance(self.getClass.getClassLoader, self.getClass.getInterfaces,
        (_, method, _) => attributes.getOrElse(method.getName, method.invoke(self))
      )
      .asInstanceOf[A]
}

/**
  * Implicit conversion for annotation operations.
  */
trait ToAnnotationOps {

  /**
    * Implicit conversion from annotation to the annotation enhancements.
    * @param a the annotational thing
    * @tparam A its type
    */
  implicit def toAnnotationOps[A <: Annotation](a: A): AnnotationOps[A] = new AnnotationOps(a)
}
