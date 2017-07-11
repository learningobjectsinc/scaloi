package scaloiz.free.tx

import scalaz.{:<:, Coproduct, Free, Id, ~>}

/**
  * Created by zpowers on 1/4/17.
  */
object InjectTest extends App {
  import UnitTransactor._

  object Transacts {
    implicit def instance[F[_]](implicit I: TxOp :<: F) = new Transacts[F]
  }
  class Transacts[F[_]](implicit I: TxOp :<: F) {
    def begin: Free[F, Transaction]                       = Free.liftF(I.inj(Begin))
    def commit(transaction: Transaction): Free[F, Unit]   = Free.liftF(I.inj(Commit(transaction)))
    def rollback(transaction: Transaction): Free[F, Unit] = Free.liftF(I.inj(Rollback(transaction)))
  }

  /** A User interaction algebra, defines operations for asking the user for input.
    */
  sealed trait InteractOp[A]
  case class Ask(prompt: String) extends InteractOp[String]
  case class Tell(msg: String)   extends InteractOp[Unit]

  object Interacts {
    implicit def instance[F[_]](implicit I: InteractOp :<: F): Interacts[F] = new Interacts[F]
  }

  /** Module for injecting Free interactions into a higher context.
    */
  class Interacts[F[_]](implicit I: InteractOp :<: F) {
    def ask(prompt: String) = Free.liftF(I.inj(Ask(prompt)))
    def tell(msg: String)   = Free.liftF(I.inj(Tell(msg)))
  }

  val console: InteractOp ~> Id.Id = new (InteractOp ~> Id.Id) {
    override def apply[A](fa: InteractOp[A]): Id.Id[A] = fa match {
      case Ask(prompt) =>
        print(prompt)
        io.StdIn.readLine()
      case Tell(msg) =>
        println(msg)
    }
  }

  def ask2commit[F[_]](implicit T: Transacts[F], I: Interacts[F]): Free[F, Unit] = {
    import I._
    import T._
    for {
      _     <- tell("Welcome to this transactor program!")
      cmd   <- ask("$")
      trans <- begin
      _ <- cmd match {
            case "c" =>
              for {
                _ <- tell("commiting")
                _ <- commit(trans)
                _ <- tell("committed")
              } yield ()
            case "r" =>
              for {
                _ <- tell("Rolling back")
                _ <- rollback(trans)
                _ <- tell("Rolled back the tx!")
              } yield ()
            case ("h" | "help" | "?") =>
              for {
                _ <- tell("h - This help message.")
                _ <- tell("c - commit the open transaction.")
                _ <- tell("r - rollback the open transaction.")
                _ <- ask2commit
              } yield ()
            case _ =>
              for {
                _ <- tell("Unknown command restarting... use h for help.")
                _ <- rollback(trans)
                _ <- ask2commit
              } yield ()
          }
    } yield ()
  }

  type App[A] = Coproduct[TxOp, InteractOp, A]
  val myApp  = ask2commit[App]
  val result = myApp.foldMap(evalId or console)
  println(result)
}
