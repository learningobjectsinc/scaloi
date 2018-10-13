package scaloi

import org.scalatest.{FlatSpec, Matchers}

class ZeroTest extends FlatSpec with Matchers {
  "Zero" should "zero" in {
    import scalaz.std.string._
    import scaloi.std.cbf._
    import scaloi.Zero._

    zero[String] shouldEqual ""
    zero[Seq[Int]] shouldEqual Seq.empty
  }
}
