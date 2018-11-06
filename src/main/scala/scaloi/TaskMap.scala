package scaloi

import scalaz.concurrent.Task
import scalaz.syntax.functor._

object TaskMap {
  /** Gather a map of tasks into a task of a map. */
  def gather[A, B](tasks: Map[A, Task[B]], exceptionCancels: Boolean = false): Task[Map[A, B]] =
    Task.gatherUnordered(tasks.toList.map(tuple => tuple._2.strengthL(tuple._1)), exceptionCancels).map(_.toMap)
}
