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

      short.zipWithDefault(long)(-1, "none") should equal (
        (1 -> "a") :: (2 -> "b") :: (-1 -> "c") :: Nil
      )

      long.tail.zipWithDefault(short ::: 3 :: Nil)("", 0) should equal (
        ("b" -> 1) :: ("c" -> 2) :: ("" -> 3) :: Nil
      )

      import scalaz.syntax.functor._
      long.zipWithDefault(long)("asdf", "fasd") should equal (long.fpair)
    }

  }

  it should "pad 'em byes" in {
    import scalaz.std.anyVal._
    import scalaz.std.list._
    import scalaz.std.string._

    val short = 1 :: 2 :: Nil
    val long = "x" :: "y" :: "z" :: Nil

    short.zipM(long) should equal (
      (1 -> "x") :: (2 -> "y") :: (0 -> "z") :: Nil
    )
  }
}
