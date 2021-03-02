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

package scaloi.misc

import scalaz.{Foldable, MonadPlus}
import scalaz.Id.Id

import scala.collection.GenTraversable
import scala.collection.generic.GenericTraversableTemplate

/** Maps flatter than flat. */
trait Hypermonad[F[_], G[_], H[_]] {

  /** Given a container of `A` and a function from `A` to a container of containers
    * of `B`, return just a container of bees.
    */
  def flatterMap[A, B](fa: F[A], f: A => G[H[B]]): F[B]
}

object Hypermonad extends LowPriHypermonad {
  implicit def gtHypermonad[F[X] <: GenTraversable[X] with GenericTraversableTemplate[X, F],
                            G[_]: Foreach,
                            H[_]: Foreach]: Hypermonad[F, G, H] = new Hypermonad[F, G, H] {
    override def flatterMap[A, B](fa: F[A], f: A => G[H[B]]): F[B] = {
      val ForeachGH = Foreach[G] compose Foreach[H]
      val builder   = fa.genericBuilder[B]
      fa.foreach { a: A =>
        ForeachGH.foreach(f(a)) { b =>
          builder += b
        }
      }
      builder.result
    }
  }

}

trait LowPriHypermonad {
  implicit def foldableHypermonad[F[_]: MonadPlus, G[_]: Foldable, H[_]: Foldable]: Hypermonad[F, G, H] =
    new Hypermonad[F, G, H] {
      override def flatterMap[A, B](fa: F[A], f: A => G[H[B]]): F[B] = {
        val FoldGH = Foldable[G] compose Foldable[H]
        MonadPlus[F].bind(fa) { a =>
          FoldGH.foldRight(f(a), MonadPlus[F].empty[B]) { (b, fb) =>
            MonadPlus[F].plus(MonadPlus[F].point(b), fb)
          }
        }
      }
    }

  implicit def weakFoldableHypermonad[F[_]: MonadPlus, G[_]: Foldable]: Hypermonad[F, G, Id] =
    new Hypermonad[F, G, Id] {
      override def flatterMap[A, B](fa: F[A], f: A => G[Id[B]]): F[B] = {
        MonadPlus[F].bind(fa) { a =>
          Foldable[G].foldRight(f(a), MonadPlus[F].empty[B]) { (b, fb) =>
            MonadPlus[F].plus(MonadPlus[F].point(b), fb)
          }
        }
      }
    }
}
