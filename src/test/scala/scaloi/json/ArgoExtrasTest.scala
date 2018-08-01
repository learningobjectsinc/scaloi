package scaloi
package json

import java.time.Instant

import org.scalatest.{EitherValues, FlatSpec, Matchers}
import scaloi.test.ScaloiTest

class ArgoExtrasTest extends FlatSpec with EitherValues with Matchers with ScaloiTest {

  behaviour of "ArgoExtras"
  import ArgoExtras._
  import argonaut._
  import Argonaut._

  it should "encode lengthy maps" in {
    Map(1L -> "one").asJson.nospaces should equal("""{"1":"one"}""")
    Map(2L -> 22222).asJson.nospaces should equal("""{"2":22222}""")
  }

  it should "decode lengthy maps" in {
    jObjectFields("1" := "a", "2" := "b")
      .as[Map[Long, String]]
      .result
      .right
      .value should equal(
      Map(1 -> "a", 2 -> "b")
    )
  }

  it should "provide plenary errors" in {
    val error = jObjectFields("adsf" := 1, "booo" := 2, "3" := 3)
      .as[Map[Long, Int]]
      .result
      .left
      .value
      ._1

    error should equal("invalid keys: (adsf, booo)")
  }

  it should "encode instants" in {
    val textInstant: String = "2018-07-19T13:00:00Z"
    val instant: Instant    = Instant.parse(textInstant)
    instant.asJson.toString() shouldEqual s""""$textInstant""""
  }

  it should "decode instants" in {
    val textInstant: String = "2018-07-19T13:00:00Z"
    jString(textInstant).as[Instant].result.right.get shouldBe Instant.parse(textInstant)
  }

}
