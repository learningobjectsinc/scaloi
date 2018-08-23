package scaloi
package json

import java.time.format.DateTimeParseException
import java.time.{Instant, LocalDateTime, ZoneOffset}

import argonaut._

import scalaz.Tree.Node
import scalaz._
import scalaz.std.stream._
import scalaz.std.string._
import scalaz.syntax.std.map._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scaloi.syntax.StringOps._

object ArgoExtras {
  import Argonaut._

  implicit final def treeCodec[V: EncodeJson: DecodeJson]: CodecJson[Tree[V]] = CodecJson(
    t => {
      Json.jObjectFields("value" := t.loc.getLabel.asJson, "children" := t.subForest.toList.asJson)
    },
    hc => {
      for {
        v        <- hc.downField("value").as[V]
        children <- hc.downField("children").as[Stream[Tree[V]]]
      } yield Node(v, children)
    }
  )

  implicit val longKeyEncoder: EncodeJsonKey[Long] = EncodeJsonKey.from(_.toString)

  implicit final val instantCodec: CodecJson[Instant] = CodecJson(
    instant => Json.jString(instant.toString), //TODO: Explicitly format to whatever postgres prefers.
    c =>
      c.as[String]
        .flatMap(
          str =>
            \/.fromTryCatchNonFatal(Instant.parse(str))
              .orElse(\/.fromTryCatchNonFatal(LocalDateTime.parse(str).toInstant(ZoneOffset.UTC)))
              .fold({
                case e: DateTimeParseException => DecodeResult.fail(e.toString, c.history)
                case e                         => throw e
              }, DecodeResult.ok))
  )

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
