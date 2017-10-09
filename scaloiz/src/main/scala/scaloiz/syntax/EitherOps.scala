package scaloiz.syntax

import scaloi.syntax.ToEitherOps

import scalaz.\/

object EitherOps extends ToEitherOps {
  /**
    * Try to evaluate a function, returning either the result or any non-fatal
    * error that was thrown. Pronounced try-ther.
    * @param f a function producing a value
    * @tparam T the function result type
    * @return either a throwable or the function result
    */
  def treither[T](f: => T): Either[Throwable, T] = \/.fromTryCatchNonFatal(f).toEither
}
