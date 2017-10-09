package scaloiz.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.util.{Failure, Success}
import scalaz.syntax.either._

class DisjunctionOpsTest extends FlatSpec with OptionValues with Matchers {
  import DisjunctionOps._

  behavior of "DisjunctionOps"

  it should "tap into values" in {

    var state = 0
    1.right <|- { s =>
      state = s
    } should equal(1.right)
    state should equal(1)
    2.left <|- { s: Int =>
      state = s
    } should equal(2.left)
    state should equal(1)
    3.right.rightTap(s => state = s) should equal(3.right)
    state should equal(3)

    4.right -<| { s: Int =>
      state = s
    } should equal(4.right)
    state should equal(3)
    5.left -<| { s =>
      state = s
    } should equal(5.left)
    state should equal(5)
    6.left.leftTap(s => state = s) should equal(6.left)
    state should equal(6)
  }

  it should "disjunct exceptions" in {
    val ex = new Exception
    treither {
      2
    } should equal(2.right)
    treither {
      throw ex
    } should equal(ex.left)
    \@~*/ {
      2
    } should equal(2.right)
    \@~*/ {
      throw ex
    } should equal(ex.left)
  }

  it should "get values when nothing is thrown" in {
    treither(2).orThrow should equal(2)
  }

  it should "throw exceptions when nothing is there" in {
    val ex = new Exception
    the[Exception] thrownBy treither(throw ex).orThrow should equal(ex)
  }

  it should "get successes when nothing is thrown" in {
    treither(2).toTry should equal(Success(2))
  }

  it should "get failures when nothing is there" in {
    val ex = new Exception
    treither(throw ex).toTry should equal(Failure(ex))
  }

  it should "try to flatMap into disjunctions possibly containing failures" in {
    val ex = new RuntimeException

    1.right[Throwable].tryFlatMap(_ => 2.right) should be(2.right)
    1.right[Throwable].tryFlatMap(_ => ex.left) should be(ex.left)
    1.right[RuntimeException].tryFlatMap(_ => { throw ex; ().right }) should be(
      ex.left)
    ex.left[Long].tryFlatMap(_ => 3L.right) should be(ex.left)
    ex.left[Long].tryFlatMap(_ => (new RuntimeException).left) should be(
      ex.left)
    ex.left[Long]
      .tryFlatMap(_ => { throw new RuntimeException; 7L.right }) should be(
      ex.left)
  }

}
