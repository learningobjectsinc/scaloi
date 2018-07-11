package scaloi.syntax

import java.{util => ju}

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.Maybe

class HypermonadOpsTest extends FlatSpec with OptionValues with Matchers {
  import HypermonadOps._
  import scalaz.std.list._
  import scalaz.std.option._
  import scaloi.misc.JavaOptionalInstances._

  behavior of "HypermonadOps"

  it should "map options flatter than flat" in {
    Option(1).flatterMap(i => Option(Option(i + 1))) shouldEqual Some(2)
    Option(1).flatterMap(i => Option(List(i + 1, i + 2))) shouldEqual Some(2) // first wins
  }

  it should "map optionals flatter than flat" in {
    ju.Optional.of("one").flatterMap(s => Option(Maybe.just(s.reverse))) shouldEqual ju.Optional.of("eno")
  }

  it should "map lists flatter than flat" in {
    List(1, 2).flatterMap(i => Option(Option(i + 1))) shouldEqual List(2, 3)
  }

  it should "map seqs flatter than flat" in {
    Seq(1, 2).flatterMap(i => Option(List(i + 1, i + 2))) shouldEqual Seq(2, 3, 3, 4)
  }
}
