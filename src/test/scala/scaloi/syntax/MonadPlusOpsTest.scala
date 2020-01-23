package scaloi
package syntax

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scaloi.test.ScaloiTest

class MonadPlusOpsTest extends AnyFlatSpec with Matchers with ScaloiTest {
  import monadPlus._

  behaviour of "MonadPlusOps"

  it should "filter zeroes" in {
    import scalaz.std.anyVal._
    import scalaz.std.option._
    import scalaz.std.list._

    Option(0).filterNZ shouldEqual None
    Option(1).filterNZ shouldEqual Some(1)

    List(0, 1, 2).filterNZ shouldEqual List(1, 2)
  }
}
