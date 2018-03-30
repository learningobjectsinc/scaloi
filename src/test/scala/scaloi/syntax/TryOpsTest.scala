package scaloi
package syntax

import org.scalatest.FlatSpec

import scala.util._

class TryOpsTest extends FlatSpec with test.ScaloiTest {

  behaviour of "TryOps"
  import TryOps._

  it should "transform errors partially" in {
    case class A(i: Int)    extends Error
    case class B(s: String) extends Error
    case class C()          extends Error

    val tf: PartialFunction[Throwable, Throwable] = {
      case A(i) => B(i.toString)
    }

    Success(1)    mapExceptions tf should equal (Success(1))
    Failure(A(2)) mapExceptions tf should equal (Failure(B("2")))
    Failure(C())  mapExceptions tf should equal (Failure(C()))
  }

}
