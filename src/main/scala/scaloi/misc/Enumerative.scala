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

import enumeratum._
import language.experimental.macros
import reflect.macros.whitebox

trait Enumerative[E <: EnumEntry] {
  val enum: Enum[E]
}

object Enumerative {
  @inline def apply[E <: EnumEntry](implicit E: Enumerative[E]): E.type = E

  implicit def ennenumeratize[E <: EnumEntry]: Enumerative[E] =
    macro ennenumeratize_impl[E]

  def ennenumeratize_impl[E: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    implicit class XtensionSymbols(sym: Symbol) { def baseClasses = sym.info.baseClasses }

    val sym = symbolOf[E]

    if (!sym.isClass)
      c.error(c.enclosingPosition, s"$sym is abstract; cannot find enum evidence for it")

    val companion = sym.companion
    assert(companion.baseClasses contains symbolOf[Enum[_]], (sym, companion, companion.baseClasses))


    q"""
       new ${symbolOf[Enumerative[_]]}[${weakTypeOf[E]}] {
         val enum: ${Ident(companion)}.type = ${Ident(companion)}
       }
     """
  }

}

