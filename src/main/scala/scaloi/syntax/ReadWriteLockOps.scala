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

import java.util.concurrent.locks.ReadWriteLock

import scala.language.implicitConversions

/**
  * Enhancements on read-write locks.
  *
  * @param self the read-write lock
  */
final class ReadWriteLockOps(val self: ReadWriteLock) extends AnyVal {
  import LockOps._

  /**
    * Perform a function while holding the read lock.
    * @param f the function
    * @tparam A the return type
    * @return the return value
    */
  def reading[A](f: => A): A = self.readLock.locked(f)

  /**
    * Perform a function while holding the write lock.
    * @param f the function
    * @tparam A the return type
    * @return the return value
    */
  def writing[A](f: => A): A = self.writeLock.locked(f)
}

/**
  * Read-write lock operations companion.
  */
object ReadWriteLockOps extends ToReadWriteLockOps

/**
  * Implicit conversion for read-write lock operations.
  */
trait ToReadWriteLockOps {

  /**
    * Implicit conversion from lock to the read-write lock enhancements.
    * @param a the lock value
    */
  implicit def toReadWriteLockOps(a: ReadWriteLock): ReadWriteLockOps = new ReadWriteLockOps(a)
}
