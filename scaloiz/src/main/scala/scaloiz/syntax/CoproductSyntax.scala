package scaloiz.syntax

import scalaz.Coproduct

/**
  * Tasty Syntax for Coproducts.
  */
object CoproductSyntax {
  type :+:[F[_], G[_]] = Coproduct[F, G, _]
}