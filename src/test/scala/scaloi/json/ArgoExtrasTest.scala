package scaloi
package json

import java.time.Instant

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scaloi.test.ScaloiTest

import scalaz.NonEmptyList

class ArgoExtrasTest extends AnyFlatSpec with EitherValues with Matchers with ScaloiTest {

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
    jString(textInstant).as[Instant].result.value shouldBe Instant.parse(textInstant)
  }

  "nelCodec" should "decode NonEmptyLists" in {
    "[1, 2]".decodeOption[NonEmptyList[Int]] shouldEqual Some(NonEmptyList(1, 2))
    "[1]".decodeOption[NonEmptyList[Int]] shouldEqual Some(NonEmptyList(1))
    "[]".decodeOption[NonEmptyList[Int]] shouldEqual None
  }

  "nelCodec" should "encode NonEmptyLists" in {
    NonEmptyList(1, 2).asJson.toString() shouldEqual "[1,2]"
    NonEmptyList(1).asJson.toString() shouldEqual "[1]"
  }

}
