/*
 * Copyright 2007 Learning Objects
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

package scaloi.free.tx

import scalaz._

/**
  * A Purely functional API for demarcating transaction boundaries.
  * The `TxOP` ADT expresses a high level operations for those boundaries.
  */
trait Transactor[T] {

  /**
    * An ADT for representing transactional operations using JTA semantics.
    */
  sealed trait TxOp[A]

  /**
    * Begin a Transaction, creating a value which represents the transaction to evaluate over.
    */
  case object Begin extends TxOp[Transaction]

  /**
    * Commit the given transaction.
    */
  case class Commit(t: Transaction) extends TxOp[Unit]

  /**
    * Rollback the given transaction.
    */
  case class Rollback(t: Transaction) extends TxOp[Unit]

  /**
    * An identity for a suspended transaction.
    */
  case class Transaction(id: Long, underlying: T)

  /**
    * A DSL for expressing transaction boundaries over a Transactor.
    */
  final class Tx[F[_]](implicit I: TxOp :<: F) {

    private[this] def lift[A](tx: TxOp[A]) = Free.liftF(I(tx))

    type TxIO[A] = Free[F, A]
    /**
      * Perform some side effect to begin a transaction.
      */
    def begin: TxIO[Transaction] = lift(Begin)

    /**
      * Commit the active transaction. Active is nebulously defined via side effects.
      */
    def commit(transaction: Transaction): TxIO[Unit] = lift(Commit(transaction))

    /**
      * Rollback the active transaction.
      */
    def rollback(transaction: Transaction): TxIO[Unit] = lift(Rollback(transaction))

    /**
      * Evaluate the given expression between the boundaries of a transaction. If the exception occurs when evaluating
      * the expression, the transaction is rolled back, else it is commited.
      */
    def perform[A](a: T => A): TxIO[Throwable \/ A] =
      for {
        transaction <- begin
        aMaybe = \/.fromTryCatchNonFatal(a(transaction.underlying))
        _ <- aMaybe.fold(_ => rollback(transaction), _ => commit(transaction))
      } yield aMaybe

    /**
      * Evaluate the given transactional effect, and perform the recovery effect given a -\/ value.
      */
    def recover[L, R](attempt: TxIO[L \/ R], recover: L => TxIO[L \/ R])(
      implicit TxIO: Monad[TxIO]): TxIO[L \/ R] =
      for {
        maybe   <- attempt
        recover <- maybe.fold(recover, _ => TxIO.point(maybe))
      } yield recover

    /**
      * Given a transactional effect, repeat the effect in the event of failure, up to the given number attempts and
      * given that the exception meets the criteria of pred.
      *
      * TODO: Maybe implement a MonadError[Tx]?
      *
      * @return
      */
    def retry[A](attempts: Int)(pred: Throwable => Boolean)(tx: TxIO[Throwable \/ A])(
      implicit TxIO: Monad[TxIO]): TxIO[Throwable \/ A] = {
      tx.flatMap(
        attempt =>
          if (attempts > 0 && attempt.isLeft && attempt.swap.forall(pred))
            retry(attempts - 1)(pred)(tx)
          else TxIO.point(attempt)) //Applicative Syntax?
    }

    /**
      * Evaluate the given list of disjunctions until a left(error) value is encountered.
      */
    def attemptOrdered[L, R](txs: List[TxIO[L \/ R]])(implicit TxIO: Monad[TxIO]): TxIO[L \/ R] =
      txs.reduce((a, b) => a.flatMap(aa => aa.fold(_ => TxIO.point(aa), _ => b)))
  }
}

/**
  * A pure transactor that performs no side effects. Useful for testing the effects
  * of different Tx abstractions.
  */
object UnitTransactor extends Transactor[Unit] {

  /**
    * Evaluate the transaction operations to their identity values.
    */
  val evalId = new (TxOp ~> Id.Id) {
    override def apply[A](fa: TxOp[A]): Id.Id[A] = fa match {
      case Begin       => unitTx
      case Commit(_)   => ()
      case Rollback(_) => ()
    }
  }

  /**
    * The unit transaction value.
    */
  val unitTx = Transaction(1L, ())
}
