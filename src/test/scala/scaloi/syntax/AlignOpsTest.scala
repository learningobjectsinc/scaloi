package scaloi
package syntax

import org.scalatest.{FlatSpec, Matchers}
import scaloi.test.ScaloiTest

class AlignOpsTest
  extends FlatSpec
     with ScaloiTest
     with Matchers {

  behaviour of "AlignOps"
  import AlignOps._

  it should "pad byes" in {
    locally {
      import scalaz.std.list._

      val short = 1 :: 2 :: Nil
      val long  = "a" :: "b" :: "c" :: Nil

      short.bipad(long)(0, "") should equal (
        (1 -> "a") :: (2 -> "b") :: (0 -> "c") :: Nil
      )

      long.tail.bipad(short ::: 3 :: Nil)("", 0) should equal (
        ("b" -> 1) :: ("c" -> 2) :: ("" -> 3) :: Nil
      )

      import scalaz.syntax.functor._
      long.bipad(long)("asdf", "fasd") should equal (long.fpair)
    }

    locally {
      import scalaz.std.map._

      val one = Map(1 -> "a", 3 -> "c")
      val two = Map(2 -> "b", 3 -> "d")

      one.bipad(two)("x", "y") should equal (Map(
        1 -> ("a" -> "y"),
        2 -> ("x" -> "b"),
        3 -> ("c" -> "d"),
      ))
    }
  }

}
