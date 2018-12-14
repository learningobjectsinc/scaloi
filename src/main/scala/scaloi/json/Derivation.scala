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
package json

import argonaut._
import Argonaut._

/** Slight boilerplate reduction for encoding/decoding sum types. */
object Derivation {

  /** Create an encoder for `Sum` which dispatches on `typeField`.
    *
    * @param typeField the string name of the field containing type information
    * @param doEncode  a function from a `Sum` to the type field value and json.
    *                  Usually this will be written with partial-function syntax
    * @return an encoder for `Sum`.
    */
  def sumEncode[Sum](typeField: String)(
    doEncode: Sum => (String, Json)
  ): EncodeJson[Sum] = EncodeJson { sum =>
    val (tpe, json) = doEncode(sum)
    json.withObject((typeField := tpe) +: _)
  }

  /** Create a decoder for `Sum` which dispatches on `typeField`.
    *
    * @param typeField the string name of the field containing type information
    * @param doDecode  a (partial) function from the value of `typeField` to a
    *                  decoder.
    */
  def sumDecode[Sum](typeField: String)(
    doDecode: PartialFunction[String, DecodeJson[_ <: Sum]]
  ): DecodeJson[Sum] = DecodeJson { hc =>
    import ArgoExtras._
    (hc --\ typeField).as[String].flatMap { tpe =>
      doDecode.lift(tpe) match {
        case Some(decoder) => decoder.decode(hc).widen
        case None          =>
          DecodeResult.fail(s"Unknown $typeField value $tpe", hc.history)
      }
    }
  }

}
