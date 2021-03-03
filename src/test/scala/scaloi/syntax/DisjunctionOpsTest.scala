package scaloi.syntax

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}
import scalaz.syntax.either._

class DisjunctionOpsTest extends AnyFlatSpec with OptionValues with Matchers {
  import ⋁._

  behavior of "DisjunctionOps"

  it should "tap into values" in {

    var state = 0
    1.right[Int] <|- { s =>
      state = s
    } should equal(1.right[Int])
    state should equal(1)
    2.left[Int] <|- { s: Int =>
      state = s
    } should equal(2.left[Int])
    state should equal(1)
    3.right[Int].rightTap(s => state = s) should equal(3.right[Int])
    state should equal(3)

    4.right[Int] -<| { s: Int =>
      state = s
    } should equal(4.right[Int])
    state should equal(3)
    5.left[Int] -<| { s =>
      state = s
    } should equal(5.left[Int])
    state should equal(5)
    6.left[Int].leftTap(s => state = s) should equal(6.left[Int])
    state should equal(6)
  }

  it should "disjunct exceptions" in {
    val ex = new Exception
    treither {
      2
    } should equal(2.right)
    treither[Int] {
      throw ex
    } should equal(ex.left[Int])
    \@~*/{
      2
    } should equal(2.right)
    \@~*/[Int] {
      throw ex
    } should equal(ex.left[Int])
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

  it should "get successes when there is a right" in {
    2.right[String].toTry(new Exception(_)) should equal(Success(2))
  }

  it should "get failures when there is a left" in {
    val ex = new Exception
    "a".left[Int].toTry(_ => ex) should equal(Failure(ex))
  }

  it should "get resolved values when nothing is thrown" in {
    treither(2).toFuture.value should equal(Some(Success(2)))
  }

  it should "get resolved failures when nothing is there" in {
    val ex = new Exception
    treither(throw ex).toFuture.value should equal(Some(Failure(ex)))
  }

  it should "get resolved values when there is a right" in {
    2.right[String].toFuture(new Exception(_)).value should equal(Some(Success(2)))
  }

  it should "get resolved failures when there is a left" in {
    val ex = new Exception
    "a".left[Int].toFuture(_ => ex).value should equal(Some(Failure(ex)))
  }

  it should "try to flatMap into disjunctions possibly containing failures" in {
    val ex = new RuntimeException

    1.right[Throwable].tryFlatMap(_ => 2.right) should be(2.right)
    1.right[RuntimeException].tryFlatMap(_ => ex.left[Int]) should be(ex.left[Int])
    1.right[RuntimeException].tryFlatMap(_ => { throw ex; ().right }) should be(
      ex.left)
    ex.left[Long].tryFlatMap(_ => 3L.right) should be(ex.left)
    ex.left[Long].tryFlatMap(_ => (new RuntimeException).left[Long]) should be(
      ex.left[Long])
    ex.left[Long]
      .tryFlatMap(_ => { throw new RuntimeException; 7L.right }) should be(
      ex.left)
  }

  it should "and then" in {
    1.left[String].andThen("ho".right[Int]) should be(1.left)
    "ha".right[Int].andThen("ho".right[Int]) should be("ho".right)
    "ha".right[Int].andThen(2.left[String]) should be(2.left)
    1.left[String] |--> "ho".right[Int] should be(1.left)
    "ha".right[Int] |--> "ho".right[Int] should be("ho".right)
  }

  it should "arrow flatmap" in {
    1.left[String] --> (_ => "ho".right[Int]) should be(1.left)
    "ha".right[Int] --> (s => s"ho$s".right[Int]) should be("hoha".right)
    "ha".right[Int] --> (_ => 2.left[String]) should be(2.left)
  }

  it should "biforeach" in {
    var a = 0
    var b = 1
    3.left[Int].biforeach(a = _, b = _)
    a shouldEqual 3
    b shouldEqual 1
    6.right[Int].biforeach(a = _, b = _)
    a shouldEqual 3
    b shouldEqual 6
  }

  it should "bifproduct" in {
    1.left[String].bifproduct(a => a + a, s => s + s) shouldEqual (1 -> 2).left
    "ha".right[Int].bifproduct(a => a + a, s => s + s) shouldEqual ("ha" -> "haha").right
  }

  it should "left flat map" in {
    1.right[Int].leftFlatMap(i => (i + 1).right[Int]) shouldEqual 1.right[Int]
    1.left[Int].leftFlatMap(i => (i + 1).right[Int]) shouldEqual 2.right[Int]
    1.left[Int].leftFlatMap(i => (i + 2).left[Int]) shouldEqual 3.left[Int]
  }
}
