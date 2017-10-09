package scaloiz.syntax

import java.util.Date

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._
import scalaz.std.iterable._
import scalaz.syntax.foldable._

class DateOpsTest extends FlatSpec with OptionValues with Matchers {
  import DateOps._

  behavior of "DateOps"

  it should "order dates" in {
    val one = new Date(10000L)
    val two = new Date(20000L)
    val three = new Date(30000L)
    List(two, one, three).maximum should equal(Some(three))
    List(two, one, three).minimum should equal(Some(one))
    List.empty[Date].maximum should equal(None)
  }
}
