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

/**
  * Enhancements on seqs.
  *
  * @param self the seq
  * @tparam A the seq value type
  */
final class SeqOps[A](val self: Seq[A]) extends AnyVal {

  /**
    * Returns whether this seq has a given size.
    * @param size the size against which to test
    * @return whether this seq has that size
    */
  def hasSize(size: Int): Boolean = self.lengthCompare(size) == 0

  /**
    * Convert each entry into a mapping from the entry index to the entry.
    *
    * @return Mapping from sequence index to value.
    */
  def mapByIndex: Map[Int, A] = self.zipWithIndex.map(_.swap).toMap
}

/**
  * Seq operations companion.
  */
object SeqOps extends ToSeqOps

/**
  * Implicit conversion for seq tag operations.
  */
trait ToSeqOps {

  /**
    * Implicit conversion from seq to the seq enhancements.
    * @param c the seq
    * @tparam C its type
    */
  implicit def toSeqOps[C](c: Seq[C]): SeqOps[C] = new SeqOps(c)
}
