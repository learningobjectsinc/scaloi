package scaloi

import scala.collection.mutable
import scala.language.higherKinds
import scalaz.{-\/, Coproduct, Free, Functor, Monad, MonadError, Show, \/-, ~>}

/**
  * Some general Natural transformations
  */
object NatTrans {

  /** Prints instances of F and G to stdOut.
    */
  def printOp[F[_], G[_]](intp: F ~> G) = log(println(_))(intp)

  def log[F[_], G[_]](logger: String => Unit)(intp: F ~> G): F ~> G = {
    new (F ~> G) {
      override def apply[A](fa: F[A]): G[A] = {
        val ga = intp(fa)
        logger(s"$fa ~> $ga")
        ga
      }
    }
  }

  /**
    * Log the result of the Effect G and it's input.
    */
  def logEff[F[_], G[_]](logger: String => Unit)(intp: F ~> G)(implicit G: Functor[G]) = new (F ~> G) {
    override def apply[A](fa: F[A]): G[A] = {
      val ga = intp(fa)
      G.map(ga)({ a =>
        logger(s"$fa ~> $a")
        a
      })
    }
  }

  /**
    * Log the result of the Effect G, any failures, and it's input.
    */
  def logErr[F[_], G[_], E: Show](logger: String => Unit)(intp: F ~> G)(implicit G: MonadError[G, E]): (F ~> G) =
    new (F ~> G) {
      override def apply[A](fa: F[A]): G[A] = {
        val logA = logEff(logger)(intp)(G)(fa)
        G.handleError(logA)({ err =>
          logger(s"$fa ~> ${Show[E].shows(err)}")
          G.raiseError(err)
        })
      }
    }

  /**
    * This should work with foldMap, need some way to prove List[F[A]] is a monad.
    */
  def aggregate[F[_]] = new (F ~> Lambda[A => List[F[A]]]) {
    override def apply[A](fa: F[A]): List[F[A]] = List(fa)
  }

  /**
    * Interpret a Free[F, A] into a H[A] with the transformation F[A] ~> H[A].
    * @see scalaz.Free#flatMapSuspension
    */
  def freeIntp[F[_], H[_]: Monad](intp: F ~> H): (Free[F, ?] ~> H) = new (Free[F, ?] ~> H) {
    override def apply[A](fa: Free[F, A]): H[A] = fa.foldMap(intp)
  }

  /**
    * Append instances of fa to mutable list.
    */
  class MutableRecorder[F[_]] extends (F ~> F) {
    val ops = mutable.Buffer.empty[F[_]]
    override def apply[A](fa: F[A]): F[A] = {
      ops += fa
      fa
    }
  }

  /**
    * A few extension methods on natural transformations.
    */
  implicit class NatTransFunctions[F[_], G[_]](self: (F ~> G)) {
    // (F ~> G) or (H ~> G) = (Coproduct(F,H) ~> G)
    def or[H[_]](f: H ~> G): Coproduct[F, H, ?] ~> G = Or(self, f)
  }

  case class Or[F[_], G[_], H[_]](fh: F ~> H, gh: G ~> H) extends (Coproduct[F, G,?] ~> H) {
    override def apply[A](c: Coproduct[F, G, A]): H[A] = c.run.fold(fh, gh)
  }
}