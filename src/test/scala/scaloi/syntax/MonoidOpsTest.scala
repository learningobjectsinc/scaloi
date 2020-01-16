package scaloi.syntax

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scaloi.test.ScaloiTest

class MonoidOpsTest
  extends AnyFlatSpec
    with Matchers
    with ScaloiTest
{

  behaviour of "MonoidOps"
  import monoid._

  it should "map, new zealand style" in {
    import scalaz.std.string._

    val explainNonEmptiness: String => String = "non-empty: " + _

    "".mapNZ(explainNonEmptiness) should equal ("")
    "string".mapNZ(explainNonEmptiness) should equal ("non-empty: string")
  }
}
