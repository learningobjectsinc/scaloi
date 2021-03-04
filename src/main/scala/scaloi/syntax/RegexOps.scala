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
package syntax

import scala.util.matching.Regex

import scala.language.implicitConversions

/** Enhancements on regex. */
final class RegexOps(private val self: Regex) extends AnyVal {
  /** Test whether this regex partially matches `s`. */
  def test(s: String): Boolean      = self.pattern.matcher(s).find

  /** Test whether this regex matches the start of `s`. */
  def lookingAt(s: String): Boolean = self.pattern.matcher(s).lookingAt

  /** Return the first matching group, if any. */
  def match1(s: String): Option[String] = PartialFunction.condOpt(s) {
    case self(e) => e
  }
}

/**
  * Implicit conversion for Regex operations.
  */
trait ToRegexOps extends Any {

  /**
    * Implicit conversion from [[scala.util.matching.Regex]] to its enhancements.
    *
    * @param r the regex
    */
  implicit def toRegexOps(r: Regex): RegexOps = new RegexOps(r)
}
