package scaloi.syntax

import scalaz.{Equal, Monoid}

/** Enhancements on [[Monoid]](s). */
final class MonoidOps[M](private val self: M) extends AnyVal {

  /** New Zealand map.
    *
    * Applies `f` to `self` if non-empty; otherwise, passes through the empty value.
    * @param f the function to map non-zero values
    * @param M the monoid instance for `M`
    * @param Me the equality instance for `M`
    * @return `self` if empty, otherwise `f(self)`
    */
  def mapNZ(f: M => M)(implicit M: Monoid[M], Me: Equal[M]): M =
    M.onNotEmpty(self)(f(self))

}

object MonoidOps extends ToMonoidOps

trait ToMonoidOps {
  import language.implicitConversions

  @inline implicit final def ToMonoidOps[M: Monoid](m: M): MonoidOps[M] =
    new MonoidOps[M](m)
}
