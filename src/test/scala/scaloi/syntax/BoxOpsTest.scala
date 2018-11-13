package scaloi.syntax

import java.{lang => jl}
import org.scalatest.{FlatSpec, Matchers}
import scaloi.test.ScaloiTest

class BoxOpsTest extends FlatSpec with Matchers with ScaloiTest {
  import BoxOps._

  behaviour of "BoxOps"

  it should "box booleans" in {
    assert(false.box eq jl.Boolean.FALSE)
    assert(true.box eq jl.Boolean.TRUE)
  }

  it should "box bytes" in {
    assert((1: Byte).box eq jl.Byte.valueOf(1: Byte))
  }

  it should "boxer shorts" in {
    assert((1: Short).box eq jl.Short.valueOf(1: Short))
  }

  it should "box ints" in {
    assert(1.box eq jl.Integer.valueOf(1))
  }

  it should "box longs" in {
    assert(1L.box eq jl.Long.valueOf(1))
  }

  it should "box floats" in {
    assert(1f.box == jl.Float.valueOf(1f))
  }

  it should "box doubles" in {
    assert(1d.box == jl.Double.valueOf(1d))
  }
}
