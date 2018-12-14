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
package syntax

import java.util.concurrent.locks.Lock

import scala.language.implicitConversions

/**
  * Enhancements on locks.
  *
  * @param self the lock
  */
final class LockOps(val self: Lock) extends AnyVal {

  /**
    * Perform a function while holding a lock.
    * @param f the function
    * @tparam A the return type
    * @return the return value
    */
  def locked[A](f: => A): A =
    try {
      self.lock()
      f
    } finally {
      self.unlock()
    }
}

/**
  * Lock operations companion.
  */
object LockOps extends ToLockOps

/**
  * Implicit conversion for lock operations.
  */
trait ToLockOps {

  /**
    * Implicit conversion from lock to the lock enhancements.
    * @param a the lock value
    */
  implicit def toLockOps(a: Lock): LockOps = new LockOps(a)
}
