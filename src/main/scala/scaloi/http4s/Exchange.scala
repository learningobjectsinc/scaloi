package scaloi.http4s

import org.http4s.client.Client
import org.http4s.{Request, Response, Uri}

import scalaz.concurrent.Task
import org.log4s._

/**
  * A typeclass describing how to map ElasticSearch Request/Response domain objects to
  * http4s Request/Responses for use with the http4s client.
  */
trait Exchange[Req, Resp] {
  import Exchange.logger
  def request(req: Req): Task[Request]
  def response(resp: Response): Task[Resp]

  def apply(host: Uri)(req: Req)(implicit client: Client): Task[Resp] = {
    for {
      _       <- Task.delay(logger.info(s"Exchanging $req with $this"))
      httpReq <- request(req)
      _       <- Task.delay(logger.info("Request:"))
      _       <- logRequest(httpReq)
      withHost = httpReq.copy(uri = Uri.resolve(host, httpReq.uri))
      resp     <- client.fetch(withHost)(logResponse(response))
    } yield resp
  }

  protected def logResponse(resp: Response): Task[Response] =
    for {
      _    <- Task.delay(logger.info(resp.toString()))
      body <- resp.body.runLog
      _    <- Task.delay(logger.info(body.mkString("\n")))
    } yield resp

  protected def logRequest(req: Request): Task[Unit] =
    for {
      _    <- Task.delay(logger.info(req.toString()))
      body <- req.bodyAsText.runLog
      _    <- Task.delay(logger.info(body.mkString("\n")))
    } yield ()

  protected def logResponse[A](f: Response => Task[A]): Response => Task[A] =
    (r: Response) =>
      for {
        _    <- Task.delay(logger.info("Response:"))
        _    <- Task.delay(logger.info(r.toString()))
        body <- r.bodyAsText.runLog
        _    <- Task.delay(logger.info(body.mkString("\n")))
        a    <- f(r)
        _    <- Task.delay(logger.info("Response Obj:"))
        _    <- Task.delay(logger.info(a.toString))
      } yield a
}
object Exchange {
  val logger: Logger = getLogger
}
