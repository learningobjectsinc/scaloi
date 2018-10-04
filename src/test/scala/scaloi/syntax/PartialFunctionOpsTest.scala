package scaloi
package syntax

import org.scalatest.{FlatSpec, Matchers}
import scaloi.test.ScaloiTest
import scalaz.std.list._

class PartialFunctionOpsTest
  extends FlatSpec
     with Matchers
     with ScaloiTest
{
  behaviour of "PartialFunctionOps"
  import PartialFunctionOps._

  it should "apply, or default" in {
    val pf: Int =?> String = { case 1 => "foo" case 2 => "bar "}
    pf.applyOrDefault(1, "blegh") should equal ("foo")
    pf.applyOrDefault(3, "blegh") should equal ("blegh")
  }

  it should "apply, or zero" in {
    val pf: Int =?> List[String] = { case 1 => "elephant" :: Nil }
    pf.applyOrZero(1) should equal ("elephant" :: Nil)
    pf.applyOrZero(2) should equal (Nil)
  }

  it should "apply em-ly" in {
    val pf: Int =?> String = { case 1 => "abbatoir" }
    pf.mapply(List(1))    should equal ("abbatoir" :: Nil)
    pf.mapply(List(1, 2)) should equal ("abbatoir" :: Nil)
    pf.mapply(List(2))    should equal (              Nil)
  }
}
