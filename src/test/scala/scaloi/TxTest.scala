package scaloi

import java.io.PrintWriter
import java.sql.Connection
import javax.sql.DataSource

import org.scalatest.FlatSpec
import scaloi.tx._

import scala.collection.mutable
import scalaz.concurrent.Task
import scalaz.{-\/, Id, \/, \/-, ~>}

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
    val prog: Tx[Throwable \/ String] = gatherOrdered(operations)
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


  class JDBCTransaction(ds: DataSource) extends (TxOp ~> Task) {
    var conn: Connection = _
    override def apply[A](fa: TxOp[A]): Task[A] = fa match {
      case Begin => Task {
        conn = ds.getConnection
        conn.setAutoCommit(false)
      }
      case Commit => Task(conn.commit())
      case Rollback => Task(conn.rollback())
      case Suspend => ???
      case Resume(id) => ???
    }
  }

  def createDataSource: DataSource = {
    val ds = new org.hsqldb.jdbc.JDBCDataSource
    ds.setURL("jdbc:hsqldb:mem:txtest;sql.syntax_pgs=true")
    ds.setUser("SA")
    ds.setPassword("")
    ds.setLogWriter(new PrintWriter(System.out))
    ds
  }

  def prepareFooTable(connection: Connection): Unit = {
    connection.createStatement().execute("CREATE TABLE Foo(Message varchar(255))")
    connection.createStatement().execute("INSERT INTO Foo VALUES ('Hello World')")
    connection.createStatement().execute("INSERT INTO Foo VALUES ('Bonjour Monde')")
  }

  def selectFoo(connection: Connection) = {
    val stmt = connection.createStatement()
    stmt.execute("SELECT * FROM Foo")
    val rs = stmt.getResultSet

    try {
      rs.next()
      rs.getString(1)
    } finally {
      rs.close()
    }
  }

  def reset(connection: Connection) = {
    connection.createStatement().execute("DROP SCHEMA PUBLIC CASCADE")
    connection.commit()
    connection.close()
  }

  "Tx and JDBC" should "perform a transaction" in {
    val ds = createDataSource
    val manager = new JDBCTransaction(ds)
    val prog = perform {
      val conn = manager.conn
      prepareFooTable(conn)
      selectFoo(conn)
    }
    val dbTask = prog.foldMap(manager).onFinish(_ => Task(reset(manager.conn)))
    val result = dbTask.unsafePerformSync

    assertResult(\/-("Hello World"))(result)
  }

  it should "rollback errors" in {
    val ds = createDataSource
    val manager = new JDBCTransaction(ds)
    val ex = new RuntimeException("Boom")
    val prog: Tx[Throwable \/ String] = perform {
      val conn = manager.conn
      prepareFooTable(conn)
      selectFoo(conn)
      throw ex
    }

    val dbTask = prog.foldMap(manager).onFinish(_ => Task(manager.conn.close()))
    val result = dbTask.unsafePerformSync
    val checkProg = perform(selectFoo(manager.conn))
    val checkTask = checkProg.foldMap(manager).onFinish(_ => Task(reset(manager.conn)))
    val checkResult = checkTask.unsafePerformSync

    println(result)
    println(checkResult)
  }
}
