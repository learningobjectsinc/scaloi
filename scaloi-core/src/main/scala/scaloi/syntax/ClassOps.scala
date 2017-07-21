package scaloi.syntax

import java.lang.annotation.Annotation

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalaz.syntax.std.`boolean`._

/**
  * Enhancements on classes.
  *
  * @param c the class instance
  * @tparam C the class type
  */
final class ClassOps[C](val c: Class[C]) extends AnyVal {
  import ClassTagOps.classTagClass

  /**
    * Get an annotation on this class.
    *
    * @tparam T the annotation type
    * @return the annotation, if present
    */
  def annotation[T <: Annotation: ClassTag]: Option[T] =
    Option(c.getAnnotation(classTagClass[T]))

  /**
    * Cast a value to this class type, if it is type compatible.
    *
    * @param o the value
    * @return the value as the target type, if it is compatible
    */
  def option(o: AnyRef): Option[C] = c.isInstance(o).option(c.cast(o))
}

/**
  * Class operations companion.
  */
object ClassOps extends ToClassOps

/**
  * Implicit conversion for class tag operations.
  */
trait ToClassOps {

  /**
    * Implicit conversion from class to the class enhancements.
    * @param c the class
    * @tparam C its type
    */
  implicit def toClassOps[C](c: Class[C]): ClassOps[C] = new ClassOps(c)
}
