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

package scaloi.free.crud

import scalaz.{:<:, Free, ~>}
import scalaz.Id.Id

import scala.collection.mutable.{Map => MMap}

/**
  * Created by zpowers on 3/25/17.
  */
sealed trait CrudAlg[Data, PK] {
  sealed trait CrudOp[T]
  case class Create(data: Data)         extends CrudOp[PK]
  case class Read(pk: PK)               extends CrudOp[Option[Data]]
  case class Update(pk: PK, data: Data) extends CrudOp[Data]
  case class Delete(pk: PK)             extends CrudOp[Unit]

  final class Crud[F[_]](implicit I: CrudOp :<: F) {
    private[this] def liftCrud[A](op: CrudOp[A]): Free[F, A] = Free.liftF(I(op))

    //Smart Constructors
    def create(data: Data): Free[F, PK]           = liftCrud(Create(data))
    def read(pk: PK): Free[F, Option[Data]]       = liftCrud(Read(pk))
    def update(pk: PK, data: Data): Free[F, Data] = liftCrud(Update(pk, data))
    def delete(pk: PK): Free[F, Unit]             = liftCrud(Delete(pk))

    //extra functions
    def getOrCreate(pk: PK, create: => Data): Free[F, Data] =
      for {
        dataMaybe <- read(pk)
      } yield dataMaybe.fold(create)(identity)
  }

  final class MutableMapStore(init: => MMap[PK, Data], pkGenerator: MMap[PK, Data] => PK) extends (CrudOp ~> Id) {
    private[this] val store = init
    override def apply[A](fa: CrudOp[A]): scalaz.Id.Id[A] = fa match {
      case Create(data) =>
        val newPK = pkGenerator(store)
        store.update(newPK, data)
        newPK
      case Read(pk) =>
        store.get(pk)
      case Update(pk, data) =>
        store.update(pk, data)
        data
      case Delete(pk) =>
        store.remove(pk)
        ()
    }
  }
}
object CrudAlg {
  implicit def apply[Data, PK]: CrudAlg[Data, PK] = new CrudAlg[Data, PK] {}
}

