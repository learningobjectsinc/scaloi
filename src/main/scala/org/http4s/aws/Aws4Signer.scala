package org.http4s
package aws

import java.nio.charset.{StandardCharsets, Charset => NioCharset}
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.BASIC_ISO_DATE
import java.time.{Instant, ZoneId}

import org.http4s.util.UrlCodingUtils
import scodec.bits._

import scalaz.Monoid
import scalaz.concurrent.Task
import scalaz.stream._
import org.log4s._

object Aws4Signer {

  val logger = getLogger

  case class Key private (id: String, secret: String)(val bytes: ByteVector)

  object Key {
    def parse(id: String, secret: String): ParseResult[Key] =
      ByteVector.fromBase64(secret) match {
        case Some(bytes) => ParseResult.success(new Key(id, secret)(bytes))
        case None        => ParseResult.fail(s"Secret was not Base64", secret)
      }
  }

  private val Method              = "AWS4-HMAC-SHA256"
  private val Charset: NioCharset = StandardCharsets.UTF_8
}

/** When you send HTTP requests to AWS, you sign the requests so that AWS can identify who sent them. You sign
  * requests with your AWS access key, which consists of an access key ID and secret access key.
  *
  * @see http://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html
  */
class Aws4Signer(key: Aws4Signer.Key, zone: String, service: String) {
  import Aws4Signer._

  private[this] def hash(bv: ByteVector) = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    bv.grouped(1024 * 16) foreach { chunk =>
      digest.update(chunk.toByteBuffer)
    }

    ByteVector(digest.digest)
  }

  private[this] def bytes(s: String) = ByteVector(s.getBytes(Charset))

  private[this] def hmac(key: ByteVector, data: ByteVector) = {
    val algo = "HmacSHA256"
    val hmac = javax.crypto.Mac.getInstance(algo)

    hmac.init(new javax.crypto.spec.SecretKeySpec(key.toArray, algo))
    ByteVector(hmac.doFinal(data.toArray))
  }

  private[this] def shortDate(instant: Instant): String = BASIC_ISO_DATE.withZone(ZoneId.of("UTC"))
    .format(instant)
    .dropRight(1) //XXX: DateTimeFormatter insist on a timezone but Amazon doesn't want one.


  private[this] val fullDateFormatter = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")

  private[this] def fullDate(instant: Instant)= fullDateFormatter.withZone(ZoneId.of("UTC")).format(instant)

  private[this] def sign(string: String, date: Instant) = {
    val kSecret  = bytes(s"AWS4${key.secret}")
    val kDate    = hmac(kSecret, bytes(shortDate(date)))
    val kRegion  = hmac(kDate, bytes(zone))
    val kService = hmac(kRegion, bytes(service))
    val kSigning = hmac(kService, bytes("aws4_request"))
    hmac(kSigning, bytes(string))
  }

  def apply(request: Request, date: Instant = Instant.now): Task[Request] = {

    request.body.runFoldMap(a => a) map { fullBody =>
      val headers = request.headers.put(
        Header("x-amz-date", fullDate(date))
      )

      val headersToSign = headers
        .put(
          Header("Host", request.uri.host.map(_.toString).getOrElse(throw new IllegalArgumentException("need a Host")))
        )
        .toList sortBy { h =>
        h.name.toString.toLowerCase
      }

      val signedHeaders = headersToSign.map(header => header.name.toString.toLowerCase).mkString(";")

      val canonicalRequest = Seq(
        request.method.name,
        UrlCodingUtils.urlEncode(toEncode = request.uri.path, spaceIsPlus = true, toSkip = UrlCodingUtils.Unreserved ++ "/"),
        request.queryString, //XXX: Does this need to be URL encoded as well?
        headersToSign
          .map({ header =>
            s"${header.name.toString.toLowerCase}:${header.value.trim}\n"
          })
          .mkString(""),
        signedHeaders,
        hash(fullBody).toHex
      ) mkString "\n"

      val stringToSign = Seq(
        Method,
        fullDate(date),
        shortDate(date) + s"/$zone/$service/aws4_request",
        hash(ByteVector(canonicalRequest.getBytes(Charset))).toHex
      ) mkString "\n"

      logger.debug(s"Canonical Request:\n$canonicalRequest")
      logger.debug(s"StringToSign:\n$stringToSign")

      val auth = Seq(
        "Credential"    -> s"${key.id}/${shortDate(date)}/$zone/$service/aws4_request",
        "SignedHeaders" -> signedHeaders,
        "Signature"     -> sign(stringToSign, date).toHex
      ) map { case (k, v) => s"$k=$v" } mkString ", "

      request.copy(
        headers = (headers.put(Header("Authorization", s"$Method $auth"))),
        body = Process.emit(fullBody)
      )
    }
  }

  private implicit val byteVectorMonoid: Monoid[ByteVector] = new Monoid[ByteVector] {
    override def zero: ByteVector = ByteVector.empty

    override def append(f1: ByteVector, f2: => ByteVector): ByteVector = f1.++(f2)
  }
}
