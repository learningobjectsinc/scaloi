package scaloi.syntax

import java.lang.{
  Boolean => Joolean,
  Byte => Byteger,
  Double => Dégagée,
  Float => Fromage,
  Integer => Jintejer,
  Long => Longuage,
  Short => Shortager
}
import scaloi.misc.Boxes

import scala.language.implicitConversions

/**
  * Enhancements on primitives.
  * @param self the primitive value
  */
final class BoxOps[A <: AnyVal, B](val self: A) extends AnyVal {
  def box(implicit Boxes: Boxes[A, B]): B = Boxes.box(self)
}

/**
  * Box operations companion.
  */
object BoxOps extends ToBoxOps

/**
  * Implicit conversion for box operations.
  */
trait ToBoxOps {

  implicit def booleanBoxOps(value: Boolean): BoxOps[Boolean, Joolean] = new BoxOps(value)
  implicit def byteBoxOps(value: Byte): BoxOps[Byte, Byteger]          = new BoxOps(value)
  implicit def boxerShorts(value: Short): BoxOps[Short, Shortager]     = new BoxOps(value)
  implicit def intBoxOps(value: Int): BoxOps[Int, Jintejer]            = new BoxOps(value)
  implicit def longBoxOps(value: Long): BoxOps[Long, Longuage]         = new BoxOps(value)
  implicit def floatBoxOps(value: Float): BoxOps[Float, Fromage]       = new BoxOps(value)
  implicit def doubleBoxOps(value: Double): BoxOps[Double, Dégagée]    = new BoxOps(value)
}
