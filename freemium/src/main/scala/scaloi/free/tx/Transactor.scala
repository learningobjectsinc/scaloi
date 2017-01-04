package scaloi.free.tx

import scalaz.{Id, _}

/**
  * A Purely functional API for demarcating transaction boundaries.
  * The `TxOP` ADT expresses a high level operations for those boundaries.
  */
trait Transactor[T] {

  /**
    * A monad for performing transactions.
    */
  type Tx[A] = Free[TxOp, A]

  /**
    * Catchable instance for Tx, useful for async streams.
    */
  implicit val catchableTx: Catchable[Tx] = new Catchable[Tx] {
    override def attempt[A](f: Tx[A]): Tx[\/[Throwable, A]] = f map { x =>
      \/-(x)
    }
    override def fail[A](err: Throwable): Tx[A] = Free.liftF(throw err)
  }

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
    * Perform some side effect to begin a transaction.
    */
  def begin: Tx[Transaction] = Free.liftF(Begin)

  /**
    * Commit the active transaction. Active is nebulously defined via side effects.
    */
  def commit(transaction: Transaction): Tx[Unit] = Free.liftF(Commit(transaction))

  /**
    * Rollback the active transaction.
    */
  def rollback(transaction: Transaction): Tx[Unit] = Free.liftF(Rollback(transaction))

  /**
    * Evaluate the given expression between the boundaries of a transaction. If the exception occurs when evaluating
    * the expression, the transaction is rolled back, else it is commited.
    */
  def perform[A](a: T => A): Tx[Throwable \/ A] =
    for {
      transaction <- begin
      aMaybe = \/.fromTryCatchNonFatal(a(transaction.underlying))
      _ <- aMaybe.fold(_ => rollback(transaction), _ => commit(transaction))
    } yield aMaybe

  /**
    * Evaluate the given transactional effect, and perform the recovery effect given a -\/ value.
    */
  def recover[L, R](attempt: Tx[L \/ R], recover: L => Tx[L \/ R])(implicit txm: Monad[Tx]): Tx[L \/ R] =
    for {
      maybe   <- attempt
      recover <- maybe.fold(recover, succ => txm.point(\/-(succ)))
    } yield recover

  /**
    * Given a transactional effect, repeat the effect in the event of failure, up to the given number attempts and
    * given that the exception meets the criteria of pred.
    *
    * TODO: Maybe implement a MonadError[Tx]?
    *
    * @return
    */
  def retry[A](attempts: Int)(pred: Throwable => Boolean)(tx: Tx[Throwable \/ A])(
      implicit txm: Monad[Tx]): Tx[Throwable \/ A] = {
    tx.flatMap(attempt =>
          if (attempts > 0 && attempt.isLeft && attempt.swap.forall(pred))
            retry(attempts - 1)(pred)(tx)
          else txm.point(attempt)) //Applicative Syntax?
  }

  /**
    * Evaluate the given list of disjunctions until a left(error) value is encountered.
    */
  def attemptOrdered[L, R](txs: List[Tx[L \/ R]])(implicit txm: Monad[Tx]): Tx[L \/ R] =
    txs.reduce((a, b) => a.flatMap(aa => aa.fold(th => txm.point(aa), _ => b)))

  /**
    * An identity for a suspended transaction.
    */
  case class Transaction(id: Long, underlying: T)
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
