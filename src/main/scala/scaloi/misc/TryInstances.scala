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
package misc

import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.{Failure => _, Success => _, _}

import scala.util._

trait TryInstances {
  implicit final val tryInstance: Monad[Try] with Traverse[Try] =
    new Monad[Try] with Traverse[Try] {
      override def point[A](a: => A): Try[A]                          = Try(a)
      override def ap[A, B](fa: => Try[A])(f: => Try[A => B]): Try[B] = fa.flatMap(a => f.map(x => x(a)))
      override def bind[A, B](fa: Try[A])(f: A => Try[B]) = fa.flatMap(f)
      override def traverseImpl[G[_], A, B](fa: Try[A])(f: A => G[B])(implicit G: Applicative[G]) =
        fa match {
          case Success(a)   => G.map(f(a))(Success(_))
          case Failure(err) => G.point(Failure(err))
        }
    }

  implicit final def tryEqual[A](implicit A: Equal[A], throwable: Equal[Throwable]): Equal[Try[A]] =
    new Equal[Try[A]] {
      def equal(ta: Try[A], tb: Try[A]) = PartialFunction.cond((ta, tb)) {
        case (Success(a), Success(b)) => A.equal(a, b)
        case (Failure(a), Failure(b)) => throwable.equal(a, b)
      }
    }

  def tryListAppend[A](fas: Try[List[A]], fbs: => Try[List[A]]): Try[List[A]] =
    fas.flatMap(as => fbs.map(bs => as ::: bs))

  implicit def tryListMonoid[A]: Monoid[Try[List[A]]] =
    Monoid.instance(tryListAppend, Success(Nil))

  implicit class TryIterableOps[A](as: List[A]) {
    def traverseTryListFF[B](f: A => Try[B]) = as.foldMap(a => f(a).map(b => List(b)))
  }
}

object TryInstances extends TryInstances {
  import Isomorphism._
  val tryIsoDisjunction: Try <~> (Throwable \/ ?) =
    new IsoFunctorTemplate[Try, Throwable \/ ?] {
      import scalaz.syntax.std.`try`._
      def to[A](fa: Try[A]) = fa.toDisjunction
      def from[A](ga: Throwable \/ A) = ga.fold(Failure(_), Success(_))
    }
}
