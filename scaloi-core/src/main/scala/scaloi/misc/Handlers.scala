package scaloi.misc

import scala.reflect.ClassTag

object Handlers {

  /**
    * Construct a `PartialFunction` which is defined only on values
    * of type `E`, for which it returns `()`.
    *
    * Use for ignoring exceptions, such as
    * ```
    * try foo()
    * catch ignoring [SpuriousException]
    * ```
    */
  def ignoring[E <: Throwable: ClassTag]: PartialFunction[Throwable, Unit] = {
    case _: E =>
  }

}
