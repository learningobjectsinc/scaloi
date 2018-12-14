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
import scalaz.syntax.functor._

object TaskMap {
  /** Gather a map of tasks into a task of a map. */
  def gather[A, B](tasks: Map[A, Task[B]], exceptionCancels: Boolean = false): Task[Map[A, B]] =
    Task.gatherUnordered(tasks.toList.map(tuple => tuple._2.strengthL(tuple._1)), exceptionCancels).map(_.toMap)
}
