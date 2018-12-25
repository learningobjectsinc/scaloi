/*
 * Copyright 2007 Cengage Learning, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scaloi
package json

import java.time.format.DateTimeParseException
import java.time.{Instant, LocalDateTime, ZoneOffset}

import argonaut._
import scalaz._
import scalaz.std.stream._
import scalaz.std.string._
import scalaz.syntax.std.list._
import scalaz.syntax.std.map._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scaloi.data.ListTree
import scaloi.syntax.string._

object ArgoExtras {
  import Argonaut._

  implicit final def treeCodec[V: EncodeJson: DecodeJson]: CodecJson[Tree[V]] = CodecJson(
    t => {
      Json.jObjectFields("value" := t.rootLabel, "children" := t.subForest)
    },
    hc => {
      for {
        v        <- hc.downField("value").as[V]
        children <- hc.downField("children").as[Stream[Tree[V]]]
      } yield Tree.Node(v, children)
    }
  )

  implicit final def strictTreeCodec[V: EncodeJson: DecodeJson]: CodecJson[StrictTree[V]] = CodecJson(
    t => {
      Json.jObjectFields("value" := t.rootLabel, "children" := t.subForest)
    },
    hc => {
      for {
        v        <- hc.downField("value").as[V]
        children <- hc.downField("children").as[Vector[StrictTree[V]]]
      } yield StrictTree.Node(v, children)
    }
  )

  implicit final def listTreeCodec[V: EncodeJson: DecodeJson]: CodecJson[ListTree[V]] = CodecJson(
    t => {
      Json.jObjectFields("value" := t.rootLabel, "children" := t.subForest)
    },
    hc => {
      for {
        v        <- hc.downField("value").as[V]
        children <- hc.downField("children").as[List[ListTree[V]]]
      } yield ListTree.Node(v, children)
    }
  )

  implicit def nelCodec[A : EncodeJson : DecodeJson]: CodecJson[NonEmptyList[A]] =
    CodecJson(
      t =>
        jArray(t.map(EncodeJson.of[A].encode).toList),
      h =>
        for {
          a   <- h.as[List[A]]
          nel <- extractNel(h.history, a)
        } yield nel
    )

  private def extractNel[A](h: CursorHistory, l: List[A]): DecodeResult[NonEmptyList[A]] =
    l.toNel.fold(
      DecodeResult.fail[NonEmptyList[A]](s"Failed to deserialize json array into a NonEmptyList, since there were no values", h)
    ) { DecodeResult.ok }

  implicit val longKeyEncoder: EncodeJsonKey[Long] = EncodeJsonKey.from(_.toString)

  def longMapCodec[V : EncodeJson : DecodeJson]: CodecJson[Map[Long, V]] =
    CodecJson.derived(longMapEncode, longMapDecode)

  implicit def longMapEncode[V: EncodeJson]: EncodeJson[Map[Long, V]] =
    EncodeJson(_.mapKeys(_.toString).asJson)

  implicit def longMapDecode[V: DecodeJson]: DecodeJson[Map[Long, V]] =
    mapDecode(_.toLong_?)

  def mapCodec[K, V: EncodeJson: DecodeJson](
    encodeKey: K => String,
    decodeKey: String => Option[K],
  ): CodecJson[Map[K, V]] = {
    def encode(m: Map[K, V]) = m.mapKeys(encodeKey).asJson
    CodecJson.derived(EncodeJson(encode), mapDecode[K, V](decodeKey))
  }

  def mapDecode[K, V: DecodeJson](decodeKey: String => Option[K]): DecodeJson[Map[K, V]] =
    DecodeJson { (hc: HCursor) =>
      hc.as[Map[String, V]].flatMap { stringMap =>
        DecodeResult.fromDisjunction(hc.history) {
          stringMap.toStream
            .traverseU({
              case (keyStr, v) => decodeKey(keyStr).toSuccessNel(keyStr).map(_ -> v)
            })
            .disjunction
            .bimap(
              invalidKeyStrs => s"invalid keys: (${invalidKeyStrs.intercalate(", ")})",
              keyValueTuples => keyValueTuples.toMap
            )
        }
      }
    }

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

  implicit class DecodeResultOps[A](private val dr: DecodeResult[A]) extends AnyVal {
    def mapHint(hinter: String => String): DecodeResult[A] =
      dr.fold((msg, ch) => DecodeResult.fail(hinter(msg), ch), DecodeResult.ok)
    def withHint(hint: String): DecodeResult[A] = mapHint(_ => hint)
    def widen[AA >: A]: DecodeResult[AA] = dr.asInstanceOf[DecodeResult[AA]]
  }

  implicit class DecodeResultCompanionOps(private val dr: DecodeResult.type) extends AnyVal {
    def fromDisjunction[A](h: CursorHistory)(disj: String \/ A): DecodeResult[A] =
      disj.fold(dr.fail(_, h), dr.ok)
  }
}
