package scaloi.misc

import scalaz.syntax.tag._
import scalaz.{@@, Monoid, Tag, \/}

/**
  * Miscellaneous monoids.
  */
object Monoids {

  /**
    * A tag for disjunctions desirous of a monoid instance which selects
    * the first left value, if any exist.
    */
  sealed trait FailFast

  /** @see [[FailFast]] */
  final val FailFast = Tag.of[FailFast]

  /**
    * Monoid instance for disjunctions that fails fast on the first left. Contrast
    * with the standard DisjunctionMonoid which requires a Semigroup left type that
    * accumulates results.
    *
    * @tparam A the left type
    * @tparam B the right type
    * @return a fail-fast monoid instance
    */
  implicit def failFastDisjunctionMonoid[A, B: Monoid]: Monoid[(A \/ B) @@ FailFast] =
    new Monoid[(A \/ B) @@ FailFast] {
      type ABFF = (A \/ B) @@ FailFast

      override def zero: ABFF = Monoid[B].zero.rightFF[A]

      override def append(l: ABFF, r: => ABFF): ABFF =
        FailFast {
          for (lv <- l.unwrap; rv <- r.unwrap) yield {
            Monoid[B].append(lv, rv)
          }
        }
    }

  implicit class FailFastMonoidSyntaxOps[T](private val self: T) extends AnyVal {
    import scalaz.syntax.either._

    /**
      * Wrap a value in the left side of a fail-fast disjunction.
      * @tparam B the right-hand type
      * @return the value, on the left
      */
    @inline def leftFF[B]: (T \/ B) @@ FailFast = FailFast(self.left)

    /**
      * Wrap a value in the right side of a fail-fast disjunction.
      * @tparam A the left-hand type
      * @return the value, on the right
      */
    @inline def rightFF[A]: (A \/ T) @@ FailFast = FailFast(self.right)
  }

}
