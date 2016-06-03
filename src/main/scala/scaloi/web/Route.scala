package scaloi.web

import scalaz.{-\/, Free, \/, \/-}

/**
  *
  */
object route {
  type Route[A] = Free[RouteOp, A]

  sealed trait RouteOp[A]
  case class Respond(request: Request) extends RouteOp[Response]
  case object Pass extends RouteOp[Unit]
  case class Error[E](e: E) extends RouteOp[E]

  def respond(request: Request): Route[Response] = Free.liftF(Respond(request))
  val pass: Route[Unit] = Free.liftF(Pass)
  def error[E](e: E): Route[E] = Free.liftF(Error(e))

  def method(method: String)(request: Request): Route[Unit] \/ Route[Response] =
    if (request.method == method)
      \/-(respond(request))
    else -\/(pass)

  case class Request(path: String, method: String)
  case class Response(statusCode: Int)
}
