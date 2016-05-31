package scaloi

import scalaz.{Catchable, Free, Id, Monad, \/, \/-, ~>}

/**
  * A Purely functional API for demarcating transaction boundaries.
  * The `TxOP` ADT expresses a high level operations for those boundaries, using
  * semantics borrowed from JTA.
  */
object tx {

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
    * Perform some side effect to begin a transaction.
    */
  case object Begin extends TxOp[Unit]

  /**
    * Commit the active transaction. Active is nebulously defined via side effects.
    */
  case object Commit extends TxOp[Unit]

  /**
    * Rollback the active transaction.
    */
  case object Rollback extends TxOp[Unit]

  /**
    * Suspend the active transaction, returning a reference to it to be resumed.
    */
  case object Suspend extends TxOp[Transaction]

  /**
    * Resume the given suspended transaction. What happens to the previous active transaction? Who knows.
    */
  case class Resume(tx: Transaction) extends TxOp[Unit]

  /**
    * Perform some side effect to begin a transaction.
    */
  def begin: Tx[Unit] = Free.liftF(Begin)

  /**
    * Commit the active transaction. Active is nebulously defined via side effects.
    */
  def commit: Tx[Unit] = Free.liftF(Commit)

  /**
    * Rollback the active transaction.
    */
  def rollback: Tx[Unit] = Free.liftF(Rollback)

  /**
    * Suspend the active transaction, returning a reference to it to be resumed.
    */
  def suspend: Tx[Transaction] = Free.liftF(Suspend)

  /**
    * Resume the given suspended transaction. What happens to the previous active transaction? Who knows.
    */
  def resume(transaction: Transaction) = Free.liftF(Resume(transaction))

  /**
    * Evaluate the given expression between the boundaries of a transaction. If the exception occurs when evaluating
    * the expression, the transaction is rolled back, else it is commited.
    */
  def perform[A](a: => A): Tx[Throwable \/ A] =
    for {
      _ <- begin
      aa = \/.fromTryCatchNonFatal(a)
      _ <- aa.fold(_ => rollback, _ => commit)
    } yield aa

  /**
    * Given a transactional effect, repeat the effect in the event of failure, up to the given number attempts and
    * given that the exception meets the criteria of pred.
    *
    * TODO: Maybe implement a MonadError[Tx]?
    * @return
    */
  def retry[A](attempts: Int)(pred: Throwable => Boolean)(
      tx: Tx[Throwable \/ A])(implicit txm: Monad[Tx]): Tx[Throwable \/ A] = {
    tx.flatMap(attempt =>
          if (attempts > 0 && attempt.isLeft && attempt.swap.forall(pred))
            retry(attempts - 1)(pred)(tx)
          else txm.point(attempt)) //Applicative Syntax?
  }

  /**
    * Evaluate the given list of disjunctions until a left(error) value is encountered.
    */
  def attemptOrdered[L, R](txs: List[Tx[L \/ R]])(
      implicit txm: Monad[Tx]): Tx[L \/ R] =
    txs.reduce((a, b) => a.flatMap(aa => aa.fold(th => txm.point(aa), _ => b)))

  /**
    * An identity for a suspended transaction.
    */
  case class Transaction(id: Long)

  /**
    * Evaluate the transaction operations to their identity values.
    */
  val evalId = new (TxOp ~> Id.Id) {
    override def apply[A](fa: TxOp[A]): Id.Id[A] = fa match {
      case Begin => ()
      case Commit => ()
      case Rollback => ()
      case Suspend => Transaction(1L)
      case Resume(_) => ()
    }
  }
}
