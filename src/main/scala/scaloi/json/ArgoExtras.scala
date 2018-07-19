package scaloi
package json

import java.time.Instant

import argonaut._
import scalaz._
import scalaz.std.stream._
import scalaz.std.string._
import scalaz.syntax.std.map._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scaloi.syntax.StringOps._

object ArgoExtras {
  import Argonaut._

  implicit val longKeyEncoder: EncodeJsonKey[Long] = EncodeJsonKey.from(_.toString)

  implicit def instantCodec: CodecJson[Instant] = {
    val encode: EncodeJson[Instant] = _.toString.asJson
    val decode: DecodeJson[Instant] = _.as[String].map(Instant.parse)
    CodecJson.derived(encode, decode)
  }

  implicit def longMapCodec[V: EncodeJson: DecodeJson]: CodecJson[Map[Long, V]] = {
    def encode(m: Map[Long, V]) = m.mapKeys(_.toString).asJson
    def decode(hc: HCursor) = hc.as[Map[String, V]].flatMap { stringMap =>
      DecodeResult.fromDisjunction(hc.history) {
        stringMap.toStream
          .traverseU {
            case (keyStr, v) => keyStr.toLong_?.toSuccessNel(keyStr).map(_ -> v)
          }
          .disjunction
          .bimap(
            badKeys => s"unacceptable non-numeric keys: (${badKeys.intercalate(", ")})",
            assocs => assocs.toMap
          )
      }
    }
    CodecJson.derived(encode, decode)
  }

  implicit class DecodeResultCompanionOps(private val dr: DecodeResult.type) {
    def fromDisjunction[A](h: CursorHistory)(disj: String \/ A): DecodeResult[A] =
      disj.fold(dr.fail(_, h), dr.ok)
  }
}
