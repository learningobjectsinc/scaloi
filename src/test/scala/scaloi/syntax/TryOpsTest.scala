package scaloi
package syntax

import org.scalatest.flatspec.AnyFlatSpec

import scala.util._
import scalaz.syntax.either._

class TryOpsTest extends AnyFlatSpec with test.ScaloiTest {

  behaviour of "TryOps"

  import `try`._

  it should "transform errors partially" in {
    case class A(i: Int) extends Error
    case class B(s: String) extends Error
    case class C() extends Error

    val tf: PartialFunction[Throwable, Throwable] = {
      case A(i) => B(i.toString)
    }

    Success(1) mapExceptions tf should equal(Success(1))
    Failure(A(2)) mapExceptions tf should equal(Failure(B("2")))
    Failure(C()) mapExceptions tf should equal(Failure(C()))
  }

  it should "provide goodly typing for successes" in {
    case class Negativity(i: Int) extends Exception(s"$i was less than zero")
    def positiveSum(is: List[Int]): Try[Int] =
      is.foldLeft(Try.success(0)) {
        case (acc, nxt) => for {
          soFar <- acc
          nxt <- if (nxt >= 0) Try.success(nxt) else Try.failure(Negativity(nxt))
        } yield soFar + nxt
      }

    positiveSum(1 :: 2 :: Nil) should equal(Success(3))
    positiveSum(1 :: -2 :: Nil) should equal(Failure(Negativity(-2)))
  }

  it should "bring things to success or failure" in {
    1.success shouldEqual Success(1)
    object err extends Throwable
    err.failure shouldEqual Failure(err)
  }

  it should "disjoin" in {
    import scalaz.syntax.either._
    object e extends Error("err")
    Success(1).disjoin(_.getMessage) should equal(1.right)
    Try.failure[Int](e).toRightDisjunction(_.getMessage) should equal("err".left)
    Try.failure[Int](e) \/> { _.getMessage } should equal("err".left)
  }

  it should "disjoin!" in {
    import scalaz.syntax.either._
    object e extends Error("err")
    Success(1) \/>| "ugh" should equal (1.right)
    Try.failure[Int](e) \/>| "ugh" should equal ("ugh".left)
  }

  it should "disjunct transformatively" in {
    Success("Yay") \/> identity shouldEqual "Yay".right
    Try.failure[Int](new Exception("Boo")) \/> { _.getMessage } shouldEqual "Boo".left
  }

  it should "disjunction" in {
    Success("Yay").disjunction shouldBe "Yay".right
    case object Boo extends Exception // want an equals implementation
    Try.failure[Int](Boo).disjunction shouldBe Boo.left
  }

  it should "left replace" in {
    object e0 extends Throwable
    object e1 extends Throwable
    Failure(e0) |<@~* e1 shouldEqual Failure(e1)
    e1.getCause shouldEqual e0
    Success(0) |<@~* e1 shouldEqual Success(0)
  }

  it should "semipartially bimap" in {
    case class A(i: Int) extends Error
    case class B(s: String) extends Error
    case class C() extends Error

    val tf: PartialFunction[Throwable, Throwable] = {
      case A(i) => B(i.toString)
    }

    Success(1) bimapf (tf, _ * 2) shouldEqual Success(2)
    Failure[Int](A(2)) bimapf (tf, _ * 2) shouldEqual Failure(B("2"))
    Failure[Int](C()) bimapf (tf, _ * 2) shouldEqual Failure(C())
  }

  behaviour of "tapFailure"

  it should "not run the side effect if the try is a Success" in {
    var x = 1
    Success(1).tapFailure(_ => x = 2) shouldBe Success(1)
    x shouldBe 1
  }

  it should "run the side effect if the try is a Failure" in {
    var x = 1
    val failure = Failure(new RuntimeException())
    failure.tapFailure(_ => x = 2) should be theSameInstanceAs failure
    x shouldBe 2
  }
}
