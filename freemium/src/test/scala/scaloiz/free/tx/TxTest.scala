package scaloiz.free.tx

import org.scalatest.FlatSpec

import scala.collection.mutable
import scalaz.{-\/, Catchable, \/, \/-}
import UnitTransactor._
import scaloiz.NatTrans.MutableRecorder

import scala.util.control.NonFatal

class TxTest extends FlatSpec {
  private[this] val dsl: Tx[TxOp] = new Tx[TxOp]
  import dsl._
  "A Free Tx monad" should "perform a transaction" in {
    val prog     = perform(_ => "Hello World")
    val recorder = new MutableRecorder[TxOp]
    val result   = prog.foldMap(recorder andThen evalId)
    assertResult(mutable.Buffer(Begin, Commit(unitTx)))(recorder.ops)
    assertResult(\/-("Hello World"))(result)
  }

  it should "rollback errors in" in {
    val ex       = new RuntimeException("Boom")
    val prog     = perform(_ => throw ex)
    val recorder = new MutableRecorder[TxOp]
    val result   = prog.foldMap(recorder andThen evalId)
    assertResult(mutable.Buffer(Begin, Rollback(unitTx)))(recorder.ops)
    assertResult(-\/(ex))(result)
  }

  it should "perform multiple transactions" in {
    import scalaz.std.list._
    import scalaz.syntax.traverse._
    val operations: List[TxIO[Throwable \/ String]] = List(
      perform(_ => "Hi"),
      perform(_ => "Hello"),
      perform(_ => "GoodBye")
    )
    val prog: TxIO[List[Throwable \/ String]] = operations.sequence[TxIO, Throwable \/ String]
    val recorder                              = new MutableRecorder[TxOp]
    val result                                = prog.foldMap(recorder andThen evalId)
    assertResult(mutable.Buffer(Begin, Commit(unitTx), Begin, Commit(unitTx), Begin, Commit(unitTx)))(recorder.ops)
    assertResult(List(\/-("Hi"), \/-("Hello"), \/-("GoodBye")))(result)
  }

  it should "rollback failed operations" in {
    import scalaz.std.list._
    import scalaz.syntax.traverse._
    val ex = new RuntimeException("Boom")
    val operations: List[TxIO[Throwable \/ String]] = List(
      perform(_ => "Hi"),
      perform(_ => throw ex),
      perform(_ => "GoodBye")
    )
    val prog: TxIO[List[Throwable \/ String]] = operations.sequence[TxIO, Throwable \/ String]
    val recorder                              = new MutableRecorder[TxOp]
    val result                                = prog.foldMap(recorder andThen evalId)
    assertResult(mutable.Buffer(Begin, Commit(unitTx), Begin, Rollback(unitTx), Begin, Commit(unitTx)))(recorder.ops)
    assertResult(List(\/-("Hi"), -\/(ex), \/-("GoodBye")))(result)
  }

  it should "halt operations when encountering an error" in {
    val ex = new RuntimeException("Boom")
    val operations: List[TxIO[Throwable \/ String]] = List(
      perform(_ => "Hi"),
      perform(_ => throw ex),
      perform(_ => "GoodBye")
    )
    val prog: TxIO[Throwable \/ String] = attemptOrdered(operations)
    val recorder                        = new MutableRecorder[TxOp]
    val result                          = prog.foldMap(recorder andThen evalId)
    assertResult(-\/(ex))(result)
    assertResult(mutable.Buffer(Begin, Commit(unitTx), Begin, Rollback(unitTx)))(recorder.ops)
  }

  it should "pass successful operations with retry" in {
    val prog     = retry(3)(_ => true)(perform(_ => "Hello World"))
    val recorder = new MutableRecorder[TxOp]
    val result   = prog.foldMap(recorder andThen evalId)
    assertResult(mutable.Buffer(Begin, Commit(unitTx)))(recorder.ops)
    assertResult(\/-("Hello World"))(result)
  }

  it should "attempt failed operations multiple times" in {
    val ex       = new RuntimeException("Boom")
    val prog     = retry(2)(_ == ex)(perform(_ => throw ex))
    val recorder = new MutableRecorder[TxOp]
    val result   = prog.foldMap(recorder andThen evalId)
    assertResult(mutable.Buffer(Begin, Rollback(unitTx), Begin, Rollback(unitTx), Begin, Rollback(unitTx)))(
      recorder.ops)
    assertResult(-\/(ex))(result)
  }

  "A Functional Streams and Tx" should "play nice" in {
    import scala.util.Random
    import scalaz.stream._

    val prog = perform(
      _ =>
        if (Random.nextBoolean()) "Hello World"
        else throw new RuntimeException("Boom"))
    val txStream = Process.eval[TxIO, Throwable \/ String](prog).repeat.takeWhile(_.isRight).runLog
    val recorder = new MutableRecorder[TxOp]
    txStream.foldMap(recorder andThen evalId)
    assert(recorder.ops.last == Rollback(unitTx))
    assert(recorder.ops.count(_ == Rollback(unitTx)) == 1)
  }

  private[this] implicit val txIOCatchable: Catchable[TxIO] = new Catchable[TxIO] {
    override def attempt[A](f: TxIO[A]): TxIO[\/[Throwable, A]] =
      try {
        f.map(\/-(_))
      } catch {
        case NonFatal(th) => fail(th)
      }

    override def fail[A](err: Throwable): TxIO[A] = rollback(unitTx).asInstanceOf[TxIO[A]]
  }
}
