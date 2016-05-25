package scaloi

import org.scalatest.FlatSpec
import scaloi.tx._

import scala.collection.mutable
import scalaz.{-\/, Free, Id, Monad, \/, \/-, ~>}

class TxTest extends FlatSpec {
  class Recorder extends (TxOp ~> Id.Id) {
    val ops = mutable.Buffer.empty[TxOp[_]]
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
    assertResult(mutable.Buffer(Begin, Commit))(recorder.ops)
    assertResult(\/-("Hello World"))(result)
  }

  it should "rollback errors in" in {
    val ex = new RuntimeException
    val prog = perform(throw ex)
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(mutable.Buffer(Begin, Rollback))(recorder.ops)
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
    val prog: Tx[List[Throwable \/ String]] =
      operations.sequence[Tx, Throwable \/ String]
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(mutable.Buffer(Begin, Commit, Begin, Commit, Begin, Commit))(
        recorder.ops)
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
    val prog: Tx[List[Throwable \/ String]] =
      operations.sequence[Tx, Throwable \/ String]
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(
        mutable.Buffer(Begin, Commit, Begin, Rollback, Begin, Commit))(
        recorder.ops)
    assertResult(List(\/-("Hi"), -\/(ex), \/-("GoodBye")))(result)
  }

  it should "halt operations when encountering an error" in {
    val ex = new RuntimeException
    val operations: List[Tx[Throwable \/ String]] = List(
        perform("Hi"),
        perform(throw ex),
        perform("GoodBye")
    )
    val prog: Tx[Throwable \/ String] = operations.reduce((a, b) =>
          a flatMap (aa =>
                aa.fold(th => implicitly[Monad[Tx]].point(aa), _ => b))) //Need a name for this operation.
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(-\/(ex))(result)
    assertResult(mutable.Buffer(Begin, Commit, Begin, Rollback))(recorder.ops)
  }

  it should "pass successful operations with retry" in {
    val prog = retry(3)(_ => true)(perform("Hello World"))
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(mutable.Buffer(Begin, Commit))(recorder.ops)
    assertResult(\/-("Hello World"))(result)
  }

  it should "attempt failed operations multiple times" in {
    val ex = new RuntimeException
    val prog = retry(2)(_ == ex)(perform(throw ex))
    val recorder = new Recorder
    val result = prog.foldMap(recorder)
    assertResult(
        mutable.Buffer(Begin, Rollback, Begin, Rollback, Begin, Rollback))(
        recorder.ops)
    assertResult(-\/(ex))(result)
  }

  "A Functional Streams and Tx" should "play nice" in {
    import scala.util.Random
    import scalaz.stream._

    val prog = perform(if (Random.nextBoolean()) "Hello World"
        else throw new RuntimeException)
    val txStream = Process
      .eval[Tx, Throwable \/ String](prog)
      .repeat
      .takeWhile(_.isRight)
      .runLog
    val recorder = new Recorder
    txStream.foldMap(recorder)
    assert(recorder.ops.last == Rollback)
    assert(recorder.ops.count(_ == Rollback) == 1)
  }
}
