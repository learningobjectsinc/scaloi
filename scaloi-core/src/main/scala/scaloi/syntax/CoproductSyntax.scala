package scaloi
package syntax

import scalaz.Coproduct

/**
  * Tasty syntax for [[scalaz.Coproduct]]s.
  */
object CoproductSyntax {
  type :+:[F[_], G[_]] = Coproduct[F, G, _]
}