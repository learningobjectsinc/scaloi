package scaloi

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.syntax.either._

import scala.collection.mutable

class GetOrCreateTest extends FlatSpec with OptionValues with Matchers {
  behavior of "GetOrCreate"

  it should "be a well-behaved structure" in {
    Gotten(1) should be('gotten)
    Gotten(1) should not be 'created
    Created(1) should be('created)
    Created(1) should not be 'gotten
    Created(1).createdOr(2) should equal(1.right)
    Gotten(1).createdOr(2) should equal(2.left)
    GetOrCreate.gotten("got").result should equal("got")
    GetOrCreate.created("crot").result should equal("crot")
    GetOrCreate
      .created(mutable.Buffer.empty[String])
      .init(_.append("crot")) should equal(Created(mutable.Buffer("crot")))
    GetOrCreate
      .gotten(mutable.Buffer.empty[String])
      .init(_.append("crot")) should equal(Gotten(mutable.Buffer.empty))
    GetOrCreate
      .created(mutable.Buffer.empty[String])
      .update(_.append("got")) should equal(Created(mutable.Buffer.empty))
    GetOrCreate
      .gotten(mutable.Buffer.empty[String])
      .update(_.append("got")) should equal(Gotten(mutable.Buffer("got")))
    GetOrCreate
      .created(mutable.Buffer.empty[String])
      .always(_.append("ev")) should equal(mutable.Buffer("ev"))
    GetOrCreate
      .gotten(mutable.Buffer.empty[String])
      .always(_.append("ev")) should equal(mutable.Buffer("ev"))
    GetOrCreate.gotten(mutable.Buffer.empty[String]) *<| { _.append("ev") } should equal(
      mutable.Buffer("ev"))
    GetOrCreate.created(1).map(_ + 1) should equal(Created(2))
    GetOrCreate.created(1).fold(_ + 1)(_ + 2) should equal(2)
    GetOrCreate.gotten(1).fold(_ + 1)(_ + 2) should equal(3)
    GetOrCreate.created(1) should matchPattern { case Created(1) => }
    val a = mutable.Buffer.empty[String]
    Created(a) foreach { _.append("crot") }
    a should equal(mutable.Buffer("crot"))
  }

}
