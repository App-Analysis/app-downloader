package de.tubs.cs.ias.labels.android

import de.tubs.cs.ias.util.googlebatch.GoogleBatchPayload
import spray.json.{JsArray, JsString, JsonParser}
import sttp.client4.Response
import sttp.client4.quick.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object LabelDownloader {

  val LANGUAGE = "EN"
  val COUNTRY = "DE"
  val GOOGLE_BATCH_EXECUTE =
    uri"https://play.google.com/_/PlayStoreUi/data/batchexecute?hl=${LANGUAGE}&gl=${COUNTRY}"

  def getPrivacyLabel(id: String): String = {
    val bodyPayload = GoogleBatchPayload.createBodyPayload(
      List(
        GoogleBatchPrivacyLabelEnvelope(
          GoogleBatchPrivacyLabelPayload(id)
        )
      )
    )
    val response: Response[String] = quickRequest
      .post(GOOGLE_BATCH_EXECUTE)
      .body(s"f.req=${URLEncoder.encode(bodyPayload, StandardCharsets.UTF_8)}")
      .contentType("application/x-www-form-urlencoded",
                   StandardCharsets.UTF_8.toString)
      .send()
    if (response.is200) {
      JsonParser(response.body.split("\n")(2)) match {
        case JsArray(elements) =>
          elements.head match {
            case JsArray(elements) =>
              assert (elements.head.asInstanceOf[JsString].value == "wrb.fr")
              assert (elements (1).asInstanceOf[JsString].value == "Ws7gDc") // this is the endpoint we specified
              val data = JsonParser (elements (2).asInstanceOf[JsString].value)
              AndroidPrivacyLabel.prettyPrint (
              AndroidPrivacyLabel (data.asInstanceOf[JsArray] ) )
            case x => throw new RuntimeException(s"expected Json Array but got ${x.getClass.toString}")
          }
        case x =>
          throw new RuntimeException(
            s"expected Json Array but got ${x.getClass.toString}")
      }
    } else {
      throw new RuntimeException(
        s"encounterd non 200 response when trying to download label $id -> ${response.code}")
    }
  }

  /*
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
  }*/

}
