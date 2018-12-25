package scaloi
package syntax

import _root_.enumeratum.{Enum, EnumEntry}
import org.scalatest.{FlatSpec, Matchers}
import scaloi.test.ScaloiTest

class EnumEntryOpsTest
  extends FlatSpec
    with Matchers
    with ScaloiTest {

  behaviour of "EnumEntryOps"
  import enumeratum._

  it should "summon enums" in {
    // disabled for boxity confusion
    //E1.A.enum should be (E1)

    (E1.B : E1).enum should be (E1)

    def sameType[Q](act: Q) =()
    //sameType[E1.type](E1.A.enum)
    sameType[E1.type]((E1.B : E1).enum)

    def indir1[E <: EnumEntry: misc.Enumerative](x: E) =
      x.enum.namesToValuesMap(x.entryName) should equal (x)

    indir1(E1.A: E1)

    """ def indir2[E <: EnumEntry](x: E) = x.enum """ shouldNot compile
  }
}

private sealed abstract class E1 extends EnumEntry

private object E1 extends Enum[E1] {
  val values = findValues

  case object A extends E1
  case object B extends E1
}
