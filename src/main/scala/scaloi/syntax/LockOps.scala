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
