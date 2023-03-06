package de.tubs.cs.ias.labels.android

import de.tubs.cs.ias.util.googlebatch.GoogleBatchPayload
import dispatch.Defaults._
import dispatch._
import spray.json.{JsArray, JsString, JsonParser}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.concurrent.Await
import scala.concurrent.duration.Duration.Inf

object LabelDownloader {

  val GOOGLE_BATCH_EXECUTE =
    "https://play.google.com/_/PlayStoreUi/data/batchexecute"
  val LANGUAGE = "EN"
  val COUNTRY = "DE"

  def getPrivacyLabel(id: String): String = {
    val body = GoogleBatchPayload.createBodyPayload(
      List(
        GoogleBatchPrivacyLabelEnvelope(
          GoogleBatchPrivacyLabelPayload(id)
        )
      )
    )
    val request = url(GOOGLE_BATCH_EXECUTE)
      .setContentType("application/x-www-form-urlencoded",
                      StandardCharsets.UTF_8)
      .addQueryParameter("hl", LANGUAGE)
      .addQueryParameter("gl", COUNTRY)
      .setMethod("POST")
      .setBody(s"f.req=${URLEncoder.encode(body, StandardCharsets.UTF_8)}")
    val res: String =
      Await.result(Http.default(request OK as.String).either, Inf) match {
        case Left(value)  => throw value
        case Right(value) => value
      }
    JsonParser(res.split("\n")(2)) match {
      case JsArray(elements) =>
        assert(elements.length == 1 + 2) //we do one request and there is 2 more f00
        elements.head match {
          case JsArray(elements) =>
            assert(elements.head.asInstanceOf[JsString].value == "wrb.fr")
            assert(elements(1).asInstanceOf[JsString].value == "Ws7gDc") // this is the endpoint we specified
            val data = JsonParser(elements(2).asInstanceOf[JsString].value)
            AndroidPrivacyLabel.prettyPrint(
              AndroidPrivacyLabel(data.asInstanceOf[JsArray]))
          case _ =>
            throw new RuntimeException(
              "the actual label content needs to be a JsArray")
        }
      case _ => throw new RuntimeException("return value needs to be an array")
    }
  }

}
