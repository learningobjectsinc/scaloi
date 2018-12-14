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

import scalaz.concurrent.Task
import scalaz.{Coproduct, Free, Monad, \/, ~>}

import scala.collection.mutable
import scala.language.higherKinds

/**
  * Some general Natural transformations
  */
object NatTrans {

  /** [[Task.fromDisjunction]] as a natural transformation. */
  val disj2Task: Throwable \/ ? ~> Task = new (Throwable \/ ? ~> Task) {
    def apply[A](fa: Throwable \/ A) = Task fromDisjunction fa
  }

  /* delay evaluation of an attempt as a Task */
  def evalDisj[F[_]](f: F ~> (Throwable \/ ?)): F ~> Task = new (F ~> Task) {
    override def apply[A](fa: F[A]): Task[A] = Task.delay(f(fa))
      .flatMap(Task.fromDisjunction)
  }

  /**
    * This should work with foldMap, need some way to prove `List[F[A]]` is a monad.
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
