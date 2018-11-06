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
