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

/**
  * An extractor template for extractors expressible as a predicate
  * and a transformation function.
  * @param pred the predicate that decides if the extractor matches
  * @param trans a function that transforms a value for which `pred` is true
  */
class Extractor[I, O](pred: I => Boolean, trans: I => O) {
  final def unapply(arg: I): Option[O] =
    if (pred(arg)) Some(trans(arg)) else None
}

/** Sundry purportedly-useful extractor templates. */
object Extractor {

  /** An extractor for strings starting with a given prefix.
    * Matches iff the string starts with `pre`.
    * Returns the part of the string after `pre`.
    */
  def dropPrefix(pre: String): Extractor[String, String] =
    new Extractor(_ startsWith pre, _ drop pre.length)

  /** An extractor for strings ending with a given suffix.
    * Matches iff the string ends with `suf`.
    * Returns the part of the string before `suf`.
    */
  def dropSuffix(pre: String): Extractor[String, String] =
    new Extractor(_ endsWith pre, _ dropRight pre.length)

}
