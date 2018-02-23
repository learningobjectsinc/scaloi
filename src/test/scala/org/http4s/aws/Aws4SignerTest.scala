package org.http4s.aws

import java.time.Instant
import java.time.format.DateTimeFormatter

import argonaut.Parse
import org.http4s._
import MediaType._
import headers._
import org.scalatest.{Assertion, FlatSpec}

/**
  * @see http://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html
  */
class Aws4SignerTest extends FlatSpec {

  "A an AwsSigner" should "sign a request" in {
    val key: ParseResult[Aws4Signer.Key] = Aws4Signer.Key.parse("AKIDEXAMPLE", "foo")
    val signer = key.map(k => new Aws4Signer(k, "us-east-1", "iam"))

    val unsigned = Request(Method.GET, Uri.unsafeFromString("https://iam.amazonaws.com/?Action=ListUsers&Version=2010-05-08"))
      .withContentType(Some(`Content-Type`(`application/x-www-form-urlencoded`, Charset.`UTF-8`)))
      .putHeaders(Host("iam.amazonaws.com"))

    val time = Instant.from(DateTimeFormatter.ofPattern("uuuuMMdd'T'HHmmssX").parse("20150830T123600Z"))

    val signed = signer.map(_(unsigned, time))

    printRequest(signed.flatMap(_.unsafePerformSyncAttempt).fold(throw _, identity))
  }


  /**
    * Compare two [[Request]]s and their payloads if applicable.
    */
  def compareRequests(expected: Request, result: Request): Assertion = {
    val eBody = expected.bodyAsText.runLog.unsafePerformSync
    val rBody = result.bodyAsText.runLog.unsafePerformSync
    info(expected.toString())
    info(eBody.mkString)
    info(result.toString())
    info(rBody.mkString)
    assertResult(expected.uri)(result.uri)
    assertResult(expected.method)(result.method)
    assertResult(eBody.map(Parse.parse))(rBody.map(Parse.parse))
    assertResult(expected.headers.toList)(result.headers.toList)
    assertResult(expected.httpVersion)(result.httpVersion)
    assertResult(expected.attributes)(result.attributes)
  }

  /**
    * Print a request to the test informer.
    */
  def printRequest(req: Request): Unit = {
    info(req.toString())
    info(req.bodyAsText.runLog.unsafePerformSync.mkString)
  }
}