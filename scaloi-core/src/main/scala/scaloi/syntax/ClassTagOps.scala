package scaloi.syntax

import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}
import scalaz.syntax.std.boolean._

/**
  * Enhancements on class tags.
  *
  * @param self the class tag instance
  * @tparam C the class type
  */
final class ClassTagOps[C](val self: ClassTag[C]) extends AnyVal {

  /**
    * Cast a value to this class type, if it is type compatible.
    *
    * @param o the value
    * @return the value as the target type, if it is compatible
    */
  def option(o: AnyRef): Option[C] =
    self.runtimeClass.isInstance(o).option(o.asInstanceOf[C])
}

/**
  * Class tag operations companion.
  */
object ClassTagOps extends ToClassTagOps {

  /**
    * Returns the runtime class of a type with ClassTag evidence.
    *
    * @tparam T the type of interest
    * @return the runtime class
    */
  def classTagClass[T: ClassTag]: Class[T] =
    classTag[T].runtimeClass.asInstanceOf[Class[T]]
}

/**
  * Implicit conversion for class tag operations.
  */
trait ToClassTagOps {

  /**
    * Implicit conversion from class tag to the class tag enhancements.
    * @param ct the class tag
    * @tparam C its type
    */
  implicit def toClassTagOps[C](ct: ClassTag[C]): ClassTagOps[C] = new ClassTagOps(ct)
}
