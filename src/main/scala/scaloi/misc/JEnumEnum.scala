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

package scaloi.misc

import java.lang.{Enum => Jnum}
import java.util.{EnumSet => JnumSet}

import scalaz.{Ordering, Enum => Znum}
import scaloi.syntax.classTag._

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

/**
  * ScalaZ enum evidence of a Java enum.
  * @param values the Java enum values
  * @tparam A the Java enum type
  */
final class JEnumEnum[A <: Jnum[A]](values: Array[A]) extends Znum[A] {
  private val n = values.length

  override def pred(a: A): A  = values((values.indexOf(a) + n - 1) % n)
  override def succ(a: A): A  = values((values.indexOf(a) + 1) % n)
  override def min: Option[A] = values.headOption
  override def max: Option[A] = values.lastOption
  override def order(x: A, y: A): Ordering =
    Ordering.fromInt(values.indexOf(x) - values.indexOf(y))

}

/**
  * Jumenum (the real slim shady) companion.
  */
object JEnumEnum {

  /**
    * Get ScalaZ enum evidence of a Java enum.
    * @tparam A the Java enum type
    * @return the evidence
    */
  implicit def jEnumEnum[A <: Enum[A]: ClassTag]: Znum[A] =
    new JEnumEnum(JnumSet.allOf(classTagClass[A]).iterator.asScala.toArray)
}
