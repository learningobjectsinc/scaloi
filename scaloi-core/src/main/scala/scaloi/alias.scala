package scaloi

object alias {

  sealed abstract class Opaque[Repr] {
    type T

    def inj(repr: Repr): T
    def prj(t: T): Repr

    def subst[F[_]](frepr: F[Repr]): F[T]
    def unsubst[F[_]](ft: F[T]): F[Repr]
  }

  object Opaque {
    def apply[T]: Opaque[T] = new Impl[T]

    private final class Impl[Repr] extends Opaque[Repr] {
      type T = Repr

      def inj(repr: Repr) = repr
      def prj(t: Repr) = t

      def subst[F[_]](frepr: F[Repr]) = frepr
      def unsubst[F[_]](ft: F[Repr]) = ft
    }
  }

  sealed abstract class Above[Repr] {
    type T <: Repr

    def inj(repr: Repr): T
    def prj(t: T): Repr

    def subst[F[_]](frepr: F[Repr]): F[T]
    def unsubst[F[_]](ft: F[T]): F[Repr]
  }

  object Above {
    def apply[T]: Above[T] = new Impl[T]

    private final class Impl[Repr] extends Above[Repr] {
      type T = Repr

      def inj(repr: Repr) = repr
      def prj(t: Repr) = t

      def subst[F[_]](frepr: F[Repr]) = frepr
      def unsubst[F[_]](ft: F[Repr]) = ft
    }
  }

  /* specialization? what specialization... see scala/bug#8405 */
  sealed abstract class AboveLong {
    type T <: Long

    def inj(repr: Long): T
    def prj(t: T): Long

    def subst[F[_]](frepr: F[Long]): F[T]
    def unsubst[F[_]](ft: F[T]): F[Long]
  }

  object AboveLong {
    def apply(): AboveLong = new Impl

    private final class Impl extends AboveLong {
      type T = Long

      def inj(repr: Long) = repr
      def prj(t: Long) = t

      def subst[F[_]](frepr: F[Long]) = frepr
      def unsubst[F[_]](ft: F[Long]) = ft
    }
  }

}

