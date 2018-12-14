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

package scaloi.syntax

import scalaz.concurrent.Task
import scalaz.{-\/, \/-}

import scala.concurrent.{Future, Promise}

/**
  * Enhancements on scalaz [[Task]]s.
  */
final class TaskOps[A](private val self: Task[A]) extends AnyVal {

  /** Execute this task and return its value in a scala [[Future]].
    */
  def unsafePerformFuture: Future[A] = {
    val promise = Promise[A]()
    self.unsafePerformAsync {
      case -\/(err) => promise.failure(err)
      case \/-(res) => promise.success(res)
    }
    promise.future
  }

}

/**
  * Enhancements on anything in support of [[Task]].
  */
final class TaskAnyOps[A](private val self: A) extends AnyVal {

  /** Convert this value to an immediate [[Task]]. */
  def now: Task[A] = Task now self
}

/**
  * Enhancements on [[Throwable]] in support of [[Task]].
  */
final class TaskThrowableOps[A <: Throwable](private val self: A) extends AnyVal {

  /** Convert this throwable to a failed [[Task]]. */
  def fail: Task[Nothing] = Task fail self
}

object TaskOps extends ToTaskOps

trait ToTaskOps {
  import language.implicitConversions

  @inline
  final implicit def ToTaskOps[A](self: Task[A]): TaskOps[A] = new TaskOps(self)

  @inline
  final implicit def ToTaskAnyOps[A](self: A): TaskAnyOps[A] = new TaskAnyOps(self)

  @inline
  final implicit def ToTaskThrowableOps[A <: Throwable](self: A): TaskThrowableOps[A] = new TaskThrowableOps(self)
}
