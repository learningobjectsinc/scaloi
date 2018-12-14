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

package scaloi.misc

import java.{util => ju}

import scalaz.{Alternative, Applicative, MonadPlus, Optional, Traverse, \/}
import scalaz.Isomorphism._
import scalaz.syntax.either._
import scalaz.syntax.std.option._

/** ju.Optional is a passable scalaz.Optional, scalaz.MonadPlus. */
trait JavaOptionalInstances {

  implicit final val javaOptionalInstance: MonadPlus[ju.Optional] with Alternative[ju.Optional] with Optional[ju.Optional] with Traverse[ju.Optional] =
    new MonadPlus[ju.Optional] with Alternative[ju.Optional] with Optional[ju.Optional] with Traverse[ju.Optional] {
      def empty[A]: ju.Optional[A] = ju.Optional.empty[A]

      def plus[A](a: ju.Optional[A], b: => ju.Optional[A]): ju.Optional[A] =
        if (a.isPresent) a else b

      def point[A](a: => A): ju.Optional[A] = ju.Optional.of(a)

      def bind[A, B](fa: ju.Optional[A])(f: A => ju.Optional[B]): ju.Optional[B] =
        fa flatMap f.apply _

      def pextract[B, A](fa: ju.Optional[A]): ju.Optional[B] \/  A =
        fa map[ju.Optional[B] \/ A] (_.right) orElse ju.Optional.empty[B].left

      def traverseImpl[F[_], A, B](fa: ju.Optional[A])(f: A => F[B])(implicit F: Applicative[F]): F[ju.Optional[B]] =
        fa map[F[ju.Optional[B]]] (a => F.map(f(a))(ju.Optional.of[B])) orElse F.point(ju.Optional.empty[B])
    }

  implicit val optionJavaOptionalIso: Option <~> ju.Optional =
    new IsoFunctorTemplate[Option, ju.Optional] {
      def to[A](fa: Option[A]): ju.Optional[A] = fa.cata(ju.Optional.of(_), ju.Optional.empty[A])
      def from[A](ga: ju.Optional[A]): Option[A] = ga.map[Option[A]](Option.apply(_)).orElse(Option.empty)
    }
}
object JavaOptionalInstances extends JavaOptionalInstances
