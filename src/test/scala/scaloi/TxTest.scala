package scaloi

import org.scalatest.FlatSpec
import scaloi.tx._
import scaloi.NatTrans.log

import scalaz.{-\/, Id, \/, \/-, ~>}
import scala.collection.mutable.Buffer

class TxTest extends FlatSpec {
  class Recorder extends (TxOp ~> Id.Id) {
    val ops = Buffer.empty[TxOp[_]]
    override def apply[A](fa: TxOp[A]): Id.Id[A] = fa match {
      case Begin =>
        ops += Begin
        ()
      case Commit =>
        ops += Commit
        ()
      case Rollback =>
        ops += Rollback
        ()
      case Suspend =>
        ops += Suspend
        Transaction(1L)
      case res @ Resume(tx) =>
        ops += res
        ()
    }
  }

  "A Free Tx monad" should "perform a transaction" in {
    val prog = perform("Hello World")
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(Buffer(Begin,Commit))(recorder.ops)
    assertResult(\/-("Hello World"))(result)
  }

  it should "rollback errors in" in {
    val ex = new RuntimeException
    val prog = perform(throw ex)
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(Buffer(Begin, Rollback))(recorder.ops)
    assertResult(-\/(ex))(result)
  }

  it should "perform multiple transactions" in {
    import scalaz.std.list._
    import scalaz.syntax.traverse._
    val operations: List[Tx[Throwable \/ String]] = List(
      perform("Hi"),
      perform("Hello"),
      perform("GoodBye")
    )
    val prog: Tx[List[Throwable \/ String]] = operations.sequence[Tx, Throwable \/ String]
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(Buffer(Begin, Commit, Begin, Commit, Begin, Commit))(recorder.ops)
    assertResult(List(\/-("Hi"), \/-("Hello"), \/-("GoodBye")))(result)
  }

  it should "rollback failed operations" in {
    import scalaz.std.list._
    import scalaz.syntax.traverse._
    val ex = new RuntimeException
    val operations: List[Tx[Throwable \/ String]] = List(
      perform("Hi"),
      perform(throw ex),
      perform("GoodBye")
    )
    val prog: Tx[List[Throwable \/ String]] = operations.sequence[Tx, Throwable \/ String]
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(Buffer(Begin, Commit, Begin, Rollback, Begin, Commit))(recorder.ops)
    assertResult(List(\/-("Hi"), -\/(ex), \/-("GoodBye")))(result)
  }

  it should "pass successful operations with retry" in {
    val prog = retry(3)(_ => true)(perform("Hello World"))
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(Buffer(Begin, Commit))(recorder.ops)
    assertResult(\/-("Hello World"))(result)
  }

  it should "attempt failed operations multiple times" in {
    val ex = new RuntimeException
    val prog = retry(2)(_ == ex)(perform(throw ex))
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(Buffer(Begin,Rollback,Begin,Rollback,Begin,Rollback))(recorder.ops)
    assertResult(-\/(ex))(result)
  }
}
