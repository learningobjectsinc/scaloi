package scaloi

import scala.language.higherKinds
import scalaz.~>

/**
 * Some general Natural transformations
 */
object NatTrans {
  /** Prints instances of F and G to stdOut.
   */
  def printOp[F[_],G[_]](intp: F ~> G) = {
    new (F ~> G) {
      override def apply[A](fa: F[A]): G[A] = fa match {
        case op =>
          val g = intp(op)
          println(s"$op ~> $g")
          g
      }
    }
  }
}
