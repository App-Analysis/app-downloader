package de.tubs.cs.ias.labels.ios

import de.tubs.cs.ias.labels.{BadTokenException, TooManyTriesException}
import scalaj.http.Http
import wvlet.log.LogSupport

import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{
  HttpClient => jClient,
  HttpRequest => jRequest,
  HttpResponse => jResponse
}
import scala.util.Random

object LabelDownloader extends LogSupport {

  private var token = getToken

  private def getToken: String = {
    val appid = 553834731
    val result = Http(s"https://apps.apple.com/de/app/id$appid")
      .header("User-Agent", "Wget/1.12 (linux-gnu)")
      .asString
    if (result.code == 403) {
      println(result)
      throw BadTokenException
    }
    val tokenString = result
    val regexp = "token%22%3A%22([^%]+)%22%7D".r.unanchored
    val token = tokenString.body match {
      case regexp(token) => token
    }
    token
  }

  final def getPrivacyLabel(appId: String, tries: Int = 4): String = {
    try {
      if (tries == 0) {
        throw TooManyTriesException
      }
      Thread.sleep(Random.nextInt(10000).toLong)
      val url =
        s"https://amp-api.apps.apple.com/v1/catalog/DE/apps/$appId?platform=iphone&extend=privacyDetails&l=en-gb"
      val req: jRequest = jRequest
        .newBuilder()
        .uri(URI.create(url))
        .setHeader("User-Agent", "curl/7.54")
        .setHeader("Origin", "https://apps.apple.com")
        .setHeader("Authorization", s"Bearer $token")
        .GET()
        .build()
      val client: jClient = jClient.newBuilder().build()
      val res: jResponse[String] = client.send(req, BodyHandlers.ofString())
      if (res.statusCode() == 404) {
        """{ msg : "404" }"""
      } else if (res.statusCode() != 200) {
        warn(s"got ${res.statusCode()}, assuming bad token. Regen and retry.")
        token = getToken
        getPrivacyLabel(appId, tries - 1)
      } else {
        val body = res.body()
        if (body.contains("Too Many Requests")) {
          warn("detected too many request feedback. 60 seconds cooldown.")
          Thread.sleep(60000)
          getPrivacyLabel(appId, tries - 1)
        } else {
          body
        }
      }
    } catch {
      case e: Exception =>
        error("something went wrong we will wait for 60 seconds and then retry")
        error(e.getMessage)
        Thread.sleep(60000)
        getPrivacyLabel(appId, tries - 1)
    }
  }

}
