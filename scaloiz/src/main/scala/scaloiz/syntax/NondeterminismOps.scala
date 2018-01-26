package scaloiz.syntax

import scalaz.{-\/, Nondeterminism, \/-}
import scalaz.syntax.monad._

final class NondeterminismOps[F[_]](val N: Nondeterminism[F]) extends AnyVal {

  /**
    * Combine two non effects non-deterministically, applying a function
    * to each side when they evaluate, and a apply a third function when they both finish,
    * returning all 3 computations as independent effects.
    */
  def wye[L, R, A, B, C](lTask: F[L], rTask: F[R])(onLeft: L => F[A],
                                                   onRight: R => F[B],
                                                   onBoth: (L, R) => F[C]): F[(F[A], F[B], F[C])] = {
    implicit val ni = N
    N.nmap2(lTask >>= (l => N.point((l, onLeft(l)))), rTask >>= (r => N.point((r, onRight(r))))) {
      case ((l, lReady), (r, rReady)) =>
        (lReady, rReady, onBoth(l, r))
    }
  }

  /**
    * [[wye]], but non-deterministically merge the 3 resulting effects into a single effect.
    */
  def flatWye[L, R, A, B, C](lTask: F[L], rTask: F[R])(onLeft: L => F[A],
                                                       onRight: R => F[B],
                                                       onBoth: (L, R) => F[C]): F[(A, B, C)] = {
    implicit val ni = N
    N.choose(lTask, rTask)
      .flatMap({
        case -\/((l, fr)) => //left finished first
          N.choose(onLeft(l), fr) //run onLeft, wait for right
            .flatMap({
              case -\/((a, fr2)) => //onLeft Finished
                fr2.flatMap({ r => //block on right
                  N.nmap2(onRight(r), onBoth(l, r))({ //run onRight and onBoth
                    case (b, c) => (a, b, c) //Return
                  })
                })
              case \/-((fa, r)) => //right finishes before onLeft
                N.nmap3(fa, onRight(r), onBoth(l, r))((_, _, _)) //wait for onLeft, onRight, and onBoth to finish.
            })
        case \/-((fl, r)) => //Right finishes first
          N.choose(fl, onRight(r)) //run onRight, wait for left
            .flatMap({
              case -\/((l, fb)) => //left finishes
                N.nmap3(onLeft(l), fb, onBoth(l, r))((_, _, _)) //run Onleft, and onBoth, wait on onRight
              case \/-((fl2, b)) => //onRight finishes first
                fl2.flatMap({ l => //block on left
                  N.nmap2(onLeft(l), onBoth(l, r))({ //run onLeft and onBoth
                    case (a, c) => (a, b, c)
                  })
                })
            })
      })
  }
}
object NondeterminismOps extends ToNondeterminismOps

trait ToNondeterminismOps {
  import language.implicitConversions

  @inline
  implicit final def toNondeterminismOps[F[_]](N: Nondeterminism[F]): NondeterminismOps[F] =
    new NondeterminismOps(N)
}
