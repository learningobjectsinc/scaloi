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

package scaloi.free.ui

import scalaz.{:<:, Free, Monad, ~>}

sealed trait ConsoleOp[A]
final case class Ask(prompt: String) extends ConsoleOp[String]
final case class Tell(msg: String) extends ConsoleOp[Unit]

final class Console[F[_]](implicit I: ConsoleOp :<: F) {
  type ConsoleIO[A] = Free[F, A]
  private[this] def lift[A](op: ConsoleOp[A]): ConsoleIO[A] = Free.liftF(I.inj(op))

  /**
    * Ask the user for input.
    */
  def ask(prompt: String):ConsoleIO[String] = lift(Ask(prompt))

  /**
    * Write a message to the user's console.
    */
  def tell(msg: String): ConsoleIO[Unit] = lift(Tell(msg))
}
object Console {
  implicit def apply[F[_]](implicit I: ConsoleOp :<: F): Console[F] = new Console[F]
}

final class ToConsole[M[_]: Monad] extends (ConsoleOp ~> M) {
  override def apply[A](fa: ConsoleOp[A]): M[A] = fa match {
    case Ask(prompt) => Monad[M].point(scala.io.StdIn.readLine(prompt))
    case Tell(msg) => Monad[M].point(scala.Console.println(msg))
  }
}
