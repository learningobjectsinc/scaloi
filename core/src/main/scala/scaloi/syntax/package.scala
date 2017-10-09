package scaloi

package object syntax {

  /**
    * The partial function that takes any value to `()`.
    */
  val constUnit: PartialFunction[Any, Unit] = PartialFunction { _ => () }

}
