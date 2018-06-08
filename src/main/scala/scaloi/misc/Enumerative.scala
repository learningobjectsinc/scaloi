package scaloi
package misc

import enumeratum._
import language.experimental.macros
import reflect.macros.whitebox

trait Enumerative[E <: EnumEntry] {
  type Super >: E <: EnumEntry
  val enum: Enum[Super]
}

object Enumerative {
  @inline def apply[E <: EnumEntry](implicit E: Enumerative[E]): E.type = E

  implicit def ennenumeratize[E <: EnumEntry]: Enumerative[E] =
    macro ennenumeratize_impl[E]

  def ennenumeratize_impl[E: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    implicit class XtensionSymbols(sym: Symbol) { def baseClasses = sym.info.baseClasses }

    val sym = symbolOf[E]
      .baseClasses
      .find(_.baseClasses.tail.headOption.contains(symbolOf[EnumEntry]))
      .getOrElse {
        c.error(c.enclosingPosition, s"${weakTypeOf[E]} is not an enum subclass")
        return EmptyTree
      }

    if (!sym.isClass)
      c.error(c.enclosingPosition, s"$sym is abstract; cannot find enum evidence for it")

    val companion = sym.companion
    assert(companion.baseClasses contains symbolOf[Enum[_]], (sym, companion, companion.baseClasses))


    q"""
       new ${symbolOf[Enumerative[_]]}[${weakTypeOf[E]}] {
         type Super = ${companion.info.baseType(symbolOf[Enum[_]]).typeArgs.head}
         val enum: ${Ident(companion)}.type = ${Ident(companion)}
       }
     """
  }

}

