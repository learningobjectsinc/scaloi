package scaloi.syntax

import java.{util => ju}

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.Maybe
import scalaz.Id.Id

class HypermonadOpsTest extends FlatSpec with OptionValues with Matchers {
  import HypermonadOps._
  import scalaz.std.list._
  import scalaz.std.option._
  import scaloi.misc.JavaOptionalInstances._

  behavior of "HypermonadOps"

  it should "map maybes flatter than flat" in {
    Maybe.just(1).flatterMap(i => Option(Option(i + 1))) shouldEqual Maybe.just(2)
  }

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

  it should "flatten lists flatter than flat" in {
    List(Option(Option(1)), Option(Option(2))).hyperFlatten shouldEqual List(1, 2)
  }

  it should "flatten maybes flatter than flat" in {
    Maybe.just(Maybe.just(Maybe.just(1))).hyperFlatten shouldEqual Maybe.just(1)
  }

  it should "flatten lists flatter than flat with different inference" in {
    List(Option(Option(1)), Option(Option(2))).hyperFlattenE shouldEqual List(1, 2)
  }

  it should "flatten maybes flatter than flat with diffenent inference" in {
    Maybe.just(Maybe.just(Maybe.just(1))).hyperFlattenE shouldEqual Maybe.just(1)
  }

  it should "map id flatter than flat" in {
    Option(1).flatterMap[Id, Option, Int](i => Some(i)) shouldEqual Some(1)
  }
}
