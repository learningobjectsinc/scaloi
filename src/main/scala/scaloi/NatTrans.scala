package scaloi

import java.util.logging.{Level, Logger}

import scala.language.higherKinds
import scalaz.{Applicative, Free, Id, ~>}

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
          println(s"$intp: $op ~> $g")
          g
      }
    }
  }

  def log[F[_], G[_]](logger: String => Unit)(intp: F ~> G) = {
    new (F ~> G) {
      override def apply[A](fa: F[A]): G[A] = fa match {
        case op =>
          val g = intp(op)
          logger(s"$intp: $op ~> $g")
          g
      }
    }
  }

  /**
    * This should work with foldMap, need some way to prove List[F[A]] is a monad.
    */
  def list[F[_]] = new (F ~> Lambda[A => List[F[A]]]) {
    override def apply[A](fa: F[A]): List[F[A]] = List(fa)
  }
}
