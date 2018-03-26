package scaloi

import scala.util.{Try, Success}

package object syntax {

  /**
    * The partial function that takes any value to `()`.
    */
  private[syntax] val constUnit: PartialFunction[Any, Unit] = { case _ => () }

  /** A successful [[Try]] containing no meaningful value.
    */
  private[syntax] val successUnit: Try[Unit] = Success(())

}
