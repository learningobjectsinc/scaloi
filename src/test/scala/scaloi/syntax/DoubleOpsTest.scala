package scaloi
package syntax

import org.scalatest._

class DoubleOpsTest
  extends FlatSpec
     with Matchers
     with test.ScaloiTest
{
  behaviour of "DoubleOps"
  import DoubleOps._

  private val exemplars = List(0, 1, -1, 100, ε, -ε, ε*2)
  private val εε = ε * 2

  it should "≈" in {
    exemplars foreach { a => withClue(a) { assert(  a ≈ a         ) } }
    exemplars foreach { a => withClue(a) { assert(  a ≈ (a + ε/2) ) } }
    exemplars foreach { a => withClue(a) { assert(  a ≈ (a - ε/2) ) } }
    exemplars foreach { a => withClue(a) { assert(!(a ≈ (a + εε) )) } }
    exemplars foreach { a => withClue(a) { assert(!(a ≈ (a - εε) )) } }
  }

  it should "⪅" in {
    exemplars foreach { a => withClue(a) { assert(  a ⪅ a         ) } }
    exemplars foreach { a => withClue(a) { assert(  a ⪅ (a + ε/2) ) } }
    exemplars foreach { a => withClue(a) { assert(  a ⪅ (a - ε/2) ) } }
    exemplars foreach { a => withClue(a) { assert( (a ⪅ (a + εε) )) } }
    exemplars foreach { a => withClue(a) { assert(!(a ⪅ (a - εε) )) } }

    def check(l: Double, r: Double): compatible.Assertion = withClue((l, r)) {
      assert(  l ⪅ r )
      assert(!(r ⪅ l))
    }
    check(0,   1)
    check(-1,  0)
    check(-1,  1)

    assert(  1 ⪅ (1- ε    ))
    assert(!(1 ⪅ (1-(ε*2))))
  }

  it should "⪆" in {
    exemplars foreach { a => withClue(a) { assert(  a ⪆ a         ) } }
    exemplars foreach { a => withClue(a) { assert(  a ⪆ (a + ε/2) ) } }
    exemplars foreach { a => withClue(a) { assert(  a ⪆ (a - ε/2) ) } }
    exemplars foreach { a => withClue(a) { assert(!(a ⪆ (a + εε) )) } }
    exemplars foreach { a => withClue(a) { assert(  a ⪆ (a - εε)  ) } }

    def check(l: Double, r: Double): compatible.Assertion = withClue((l, r)) {
      assert(  r ⪆ l )
      assert(!(l ⪆ r))
    }
    check(0,   1)
    check(-1,  0)
    check(-1,  1)

    assert(  1 ⪆ (1+ ε    ))
    assert(!(1 ⪆ (1+(ε*2))))
  }

}
