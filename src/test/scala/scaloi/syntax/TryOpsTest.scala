package scaloi
package syntax

import org.scalatest.FlatSpec

import scala.util._
import scalaz.syntax.either._

class TryOpsTest extends FlatSpec with test.ScaloiTest {

  behaviour of "TryOps"

  import TryOps._

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
    Failure(e).disjoin(_.getMessage) should equal("err".left)
    Failure(e) \/> { _.getMessage } should equal("err".left)
  }

  it should "disjoin!" in {
    import scalaz.syntax.either._
    object e extends Error("err")
    Success(1) \/>! "ugh" should equal (1.right)
    Failure(e) \/>! "ugh" should equal ("ugh".left)
  }

  it should "disjunct transformatively" in {
    Success("Yay") \/> identity shouldEqual "Yay".right
    Failure(new Exception("Boo")) \/> { _.getMessage } shouldEqual "Boo".left
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
