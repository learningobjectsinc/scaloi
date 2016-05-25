package scaloi

import scalaz.{Catchable, Free, Monad, \/, \/-}

/**
  * Created by zpowers on 5/20/16.
  */
object tx {

  /**
    * A monad for performing transactions
    */
  type Tx[A] = Free[TxOp, A]

  /**
    * Catchable instance for Tx, useful for async streams.
    */
  implicit val catchableTx: Catchable[Tx] = new Catchable[Tx] {
    override def attempt[A](f: Tx[A]): Tx[\/[Throwable, A]] = f map {x => \/-(x) }
    override def fail[A](err: Throwable): Tx[A] = Free.liftF(throw err)
  }
  /**
    * An ADT for representing transactional operations using JTA semantics.
    */
  sealed trait TxOp[A]
  case object Begin extends TxOp[Unit]
  case object Commit extends TxOp[Unit]
  case object Rollback extends TxOp[Unit]
  case object Suspend extends TxOp[Transaction]
  case class Resume(tx: Transaction) extends TxOp[Unit]

  def begin: Tx[Unit] = Free.liftF(Begin)
  def commit: Tx[Unit] = Free.liftF(Commit)
  def rollback: Tx[Unit] = Free.liftF(Rollback)
  def suspend: Tx[Transaction] = Free.liftF(Suspend)

  def perform[A](a: => A): Tx[Throwable \/ A] =
    for {
      _ <- begin
      aa = \/.fromTryCatchNonFatal(a)
      _ <- aa.fold(_ => rollback, _ => commit)
    } yield aa

  /**
    * Given a tx, repeat the effect in the event of failure, up to the given number attempts and given that the exception
    * meets the criteria of pred.
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

  case class Transaction(id: Long)
}
