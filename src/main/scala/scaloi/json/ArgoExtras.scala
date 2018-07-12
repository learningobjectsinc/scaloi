package scaloi.json

import argonaut.{CodecJson, DecodeJson, EncodeJson, EncodeJsonKey}
import scalaz.syntax.std.map._

object ArgoExtras {
  implicit val longKeyEncoder: EncodeJsonKey[Long] = EncodeJsonKey.from(_.toString)

  implicit def longMapCodec[V: EncodeJson: DecodeJson]: CodecJson[Map[Long, V]] =
    CodecJson.derived[Map[String, V]].xmap(_.mapKeys(_.toLong))(_.mapKeys(_.toString))
}
