package scaloi

import scalaz.{Applicative, Bitraverse, \/}
import scalaz.syntax.either._

/** Either, neither or both type. Represents an [[A]], a [[B]], both an [[A]] and a [[B]],
  * or neither. Contrast with [[scalaz.\&/]] which does not admit the possibility of
  * neither and [[scalaz.\/]] which further denies the possibility of both. This is
  * isomorphic with [[Option[scalaz.\&/]]] but less opaque.
  *
  * @tparam A the left possible type
  * @tparam B the right possible type
  */
sealed abstract class \|/[+A, +B] extends Product with Serializable {
  import \|/._

  /** Get this, if present. */
  def thisOption: Option[A] = PartialFunction.condOpt(this) {
    case This(a)    => a
    case Both(a, _) => a
  }

  /** Get that, if present. */
  def thatOption: Option[B] = PartialFunction.condOpt(this) {
    case That(b)    => b
    case Both(_, b) => b
  }

  /** Get either, if present, but not both. */
  def eitherOption: Option[A \/ B] = PartialFunction.condOpt(this) {
    case This(a) => a.left
    case That(b) => b.right
  }

  /** Map this and that. */
  def bimap[C, D](f: A => C, g: B => D): C \|/ D =
    \|/(thisOption map f, thatOption map g)

  /** Traverse this and that. */
  def bitraverse[F[_]: Applicative, C, D](f: A => F[C], g: B => F[D]): F[C \|/ D] = this match {
    case Neither    => Applicative[F].point(Neither)
    case This(a)    => Applicative[F].apply(f(a))(This.apply)
    case That(b)    => Applicative[F].apply(g(b))(That.apply)
    case Both(a, b) => Applicative[F].apply2(f(a), g(b))(Both.apply)
  }

  /** Is this only one of the two values? */
  def isOnlyOne: Boolean = this match {
    case This(_) | That(_) => true
    case _                 => false
  }
}

/** Either, neither or both companion. */
object \|/ {

  /**
    * Construct an either, neither or both from a pair of options.
    * @param ao this value, if present
    * @param bo that value, if present
    * @tparam A this type
    * @tparam B that type
    * @return the either, neither or both
    */
  def apply[A, B](ao: Option[A], bo: Option[B]): A \|/ B = (ao, bo) match {
    case (None, None)       => Neither
    case (Some(a), None)    => This(a)
    case (None, Some(b))    => That(b)
    case (Some(a), Some(b)) => Both(a, b)
  }

  /** When neither option is present. */
  case object Neither extends (Nothing \|/ Nothing)

  /** When only this value is present. */
  final case class This[A](a: A) extends (A \|/ Nothing)

  /** When only that value is present. */
  final case class That[B](b: B) extends (Nothing \|/ B)

  /** When both values are present. */
  final case class Both[A, B](a: A, b: B) extends (A \|/ B)

  /**
    * Bitraverse evidence for ENBs.
    */
  implicit val BitraverseENB: Bitraverse[\|/] = new Bitraverse[\|/] {
    override def bimap[A, B, C, D](ab: A \|/ B)(f: A => C, g: B => D): C \|/ D = ab bimap (f, g)

    override def bitraverseImpl[F[_]: Applicative, A, B, C, D](ab: A \|/ B)(f: A => F[C], g: B => F[D]): F[C \|/ D] =
      ab.bitraverse(f, g)
  }
}
