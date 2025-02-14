package de.tubs.cs.ias.applist.android

import de.tubs.cs.ias.applist.android.AppListParser.{
  readInAppBatchResponse => androidReadInAppBatchResponse
}
import de.tubs.cs.ias.applist.AppListParser.appListFormat
import de.tubs.cs.ias.util.googlebatch.GoogleBatchPayload
import spray.json.{JsonParser, enrichAny}
import sttp.client4.Response
import sttp.client4.quick.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object AppListDownloader {

  val COUNTRY = "DE"
  val LANGUAGE = "EN"
  val CHART = "topselling_free"
  val COUNT = 200 // iOS standard is also 200

  val GOOGLE_BATCH_EXECUTE =
    uri"https://play.google.com/_/PlayStoreUi/data/batchexecute?hl=${LANGUAGE}&gl=${COUNTRY}"

  /*
  def download(id: String): String = {
    val stuff = GoogleBatchPayload.createBodyPayload(
      List(
        GoogleBatchTopChartEnvelope(
          GoogleBatchTopChartPayload(id, CHART, COUNT))))
    val request = url(GOOGLE_BATCH_EXECUTE)
      .setContentType("application/x-www-form-urlencoded",
                      StandardCharsets.UTF_8)
      .addQueryParameter("hl", LANGUAGE)
      .addQueryParameter("gl", COUNTRY)
      .setMethod("POST")
      .setBody(s"f.req=${URLEncoder.encode(stuff, StandardCharsets.UTF_8)}")
    val res: String =
      Await.result(Http.default(request OK as.String).either, Inf) match {
        case Left(value)  => throw value
        case Right(value) => value
      }
    androidReadInAppBatchResponse(JsonParser(res.substring(4).trim)).toJson.prettyPrint
  }*/

  def download(id: String): String = {
    val bodyPayload: String = GoogleBatchPayload.createBodyPayload(
      List(
        GoogleBatchTopChartEnvelope(
          GoogleBatchTopChartPayload(id, CHART, COUNT))))
    val resonse: Response[String] =
      quickRequest
        .post(GOOGLE_BATCH_EXECUTE)
        .body(
          s"f.req=${URLEncoder.encode(bodyPayload, StandardCharsets.UTF_8)}")
        .contentType("application/x-www-form-urlencoded",
                     StandardCharsets.UTF_8.toString)
        .send()
    if (resonse.is200) {
      androidReadInAppBatchResponse(JsonParser(resonse.body.substring(4).trim)).toJson.prettyPrint
    } else {
      throw new RuntimeException(
        s"non 200 response when trying to download chart $id -> ${resonse.code}")
    }
  }

}
