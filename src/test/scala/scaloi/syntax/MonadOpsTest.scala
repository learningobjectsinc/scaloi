package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.std.option._

class MonadOpsTest extends FlatSpec with OptionValues with Matchers {
  import monad._

  behavior of "MonadOps"

  it should "flatTap" in {

    def none[A]: Option[A] = None
    def some[A](a: A): Option[A] = Some(a)

    none[Int].flatTap(_ => None) should be (None)
    none[Symbol].flatTap(_ => Some(5)) should be (None)
    some("bop").flatTap(_ => None) should be (None)
    some("beep").flatTap(_ => Some('boop)) should be (Some("beep"))

    import scalaz.syntax.either._

    "bzzt".left[Int].flatTap(i => (i * 2).right) should be ("bzzt".left)
    "bork".left[Int].flatTap(i => "bumpus".left) should be ("bork".left)
    "bark".right[Symbol].flatTap(i => (i ++ i).right) should be ("bark".right)
    "biribiri".right[Symbol].flatTap(i => 'blimp.left) should be ('blimp.left)


  }

}
