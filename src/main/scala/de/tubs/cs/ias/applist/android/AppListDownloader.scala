package de.tubs.cs.ias.applist.android

import de.tubs.cs.ias.applist.android.AppListParser.{
  readInAppBatchResponse => androidReadInAppBatchResponse
}
import de.tubs.cs.ias.applist.AppListParser.appListFormat
import de.tubs.cs.ias.util.googlebatch.GoogleBatchPayload
import dispatch.Defaults._
import dispatch._
import spray.json.{JsonParser, enrichAny}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.concurrent.Await
import scala.concurrent.duration.Duration.Inf

object AppListDownloader {

  val COUNTRY = "DE"
  val LANGUAGE = "EN"
  val CHART = "topselling_free"
  val COUNT = 200 // iOS standard is also 200

  val GOOGLE_BATCH_EXECUTE =
    "https://play.google.com/_/PlayStoreUi/data/batchexecute"

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
  }

}
