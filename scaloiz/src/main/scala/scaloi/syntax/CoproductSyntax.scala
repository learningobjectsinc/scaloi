package scaloi.syntax

import scalaz.Coproduct

/**
  * Tasty Syntax for Coproducts.
  */
class CoproductSyntax {
  type :+:[F[_], G[_]] = Lambda[A =>Coproduct[F, G, A]]
}
