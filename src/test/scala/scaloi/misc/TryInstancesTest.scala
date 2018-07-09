package scaloi.misc

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest._
import scalaz.{Success =>_, Failure => _, _}
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.list._
import scaloi.test.ScaloiTest

import scala.util.{Failure, Success, Try}

class TryInstancesTest
    extends FlatSpec
       with Matchers
       with OptionValues
       with prop.Checkers
       with ScaloiTest
       with TryArbitraries
{
  import TryInstances._

  "Try List" should "fail fast" in {
    val values: List[Int]               = List(0, 1, 2, 3)
    val triedValues: Try[Iterable[Int]] = values.traverseTryListFF(lessThanOne)

    triedValues match {
      case Failure(iae: IllegalArgumentException) => iae.getMessage shouldBe "I don't like 1"
      case _                                      => fail("Should have been Failure")
    }
  }

  it should "should succeed" in {
    val values: List[Int]          = List(0, 0, 0)
    val triedValues: Try[Seq[Int]] = values.traverseTryListFF(lessThanOne)

    triedValues match {
      case Success(ints) => ints.size shouldBe 3
      case _             => fail("Should have succeeded")
    }
  }

  behaviour of "Try"
  import Prop._

  it should "be a (sad) Monad" in {
    val law = tryInstance.monadLaw; import law._
    type A = Int; type B = String; type C = Long
    type F[X] = Try[X]

    // Monad
    check(forAll((fa: F[A]) => rightIdentity(fa)))
    check(forAll((a: A, f: A => F[B]) => leftIdentity(a, f)))

    // Applicative
    check(forAll((fa: F[A]) => identityAp(fa)))
    check(forAll((ab: A => B, a: A) => homomorphism(ab, a)))
    check(forAll((fab: F[A => B], a: A) => interchange(fab, a)))
    check(forAll((f: A => B, fa: F[A]) => mapLikeDerived(f, fa)))

    // Apply
    check(forAll((fbc: F[B => C], fab: F[A => B], fa: Try[A]) => composition(fbc, fab, fa)))

    // Bind
    check(forAll((fa: F[A], f: A => F[B], g: B => F[C]) => associativeBind(fa, f, g)))
    check(forAll((fa: F[A], f: F[A => B]) => apLikeDerived(fa, f)))

    // Functor
    check(forAll((fa: F[A]) => identity(fa)))
    check(forAll((fa: F[A], f1: A => B, f2: B => C) => composite(fa, f1, f2)))
  }

  it should "be a (bad) Traversable" in {
    val law = tryInstance.traverseLaw; import law._
    type A = Int; type B = String; type C = Long
    type F[X] = Try[X]; type N[X] = List[X]; type M[X] = Option[X]

    implicit val optionMonad: Monad[Option] = Scalaz.optionInstance
    implicit val thatThing = Equal.equalA[M[N[Try[C]]]]
    implicit val otherThing = Equal.equalA[(M[Try[B]], N[Try[B]])]
    val nat = new (Option ~> List) { def apply[A](f: Option[A]) = f.toList }

    check(forAll((fa: F[A], f: A => B) => identityTraverse(fa, f)))
    check(forAll((fa: F[A], amb: A => M[B], bnc: B => N[C]) => sequentialFusion(fa, amb, bnc)))
    check(forAll((fa: F[A]) => purity[N, A](fa)))
    check(forAll((fma: F[M[A]]) => naturality(nat)(fma))) // meh
    check(forAll((fa: F[A], amb: A => M[B], anb: A => N[B]) => parallelFusion(fa, amb, anb)))

  }

  def lessThanOne(x: Int): Try[Int] = {
    if (x < 1) {
      Success(x)
    } else if (x == 1) {
      Failure(new IllegalArgumentException("I don't like 1"))
    } else {
      Failure(new IllegalArgumentException("I don't like large numbers"))
    }
  }

  implicit val defaultThrowableEqual: Equal[Throwable] = Equal.equalA
}

trait TryArbitraries {
  final def tryGen[A](A: Gen[A]): Gen[Try[A]] = {
    val failure = Gen.const(Failure(Kaboom))
    val success = A.map(Success(_))
    Gen.oneOf(failure, success)
  }

  implicit final def tryArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Try[A]] =
    Arbitrary(tryGen(A.arbitrary))

  final object Kaboom extends Throwable
}
