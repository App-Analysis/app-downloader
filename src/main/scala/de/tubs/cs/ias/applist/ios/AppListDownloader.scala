package de.tubs.cs.ias.applist.ios

import de.tubs.cs.ias.applist.AppListParser.appListFormat
import scalaj.http.Http
import wvlet.log.LogSupport
import de.tubs.cs.ias.applist.ios.AppListParser.{
  readInAppList => iosReadInDownloadedAppList
}
import spray.json.{JsonParser, enrichAny}

object AppListDownloader extends LogSupport {

  var CHART_LIST_URL =
    "https://itunes.apple.com/WebObjects/MZStore.woa/wa/viewTop"

  var HEADER_COUNTRY = "143443" // Germany
  var HEADER_LANGUAGE = "4" // German
  var HEADER_PLATFORM = "26" // give me JSON

  /** download a top 200 chart for a given genre
    *
    * Will use country Germany and language German
    *
    * @param genreId the id determining the genre of the apps to be downloaded
    * @param cc the country code
    * @param language the language of the apps
    * @param popId the type of chart
    */
  def download(genreId: String,
               cc: String = "de",
               language: String = "de",
               popId: String = "27",
               headerCountry: String = HEADER_COUNTRY,
               headerLanguage: String = HEADER_LANGUAGE,
               headerPlatform: String = HEADER_PLATFORM): String = {
    val result = Http(CHART_LIST_URL)
      .header("X-Apple-Store-Front",
              s"$headerCountry-$headerLanguage,$headerPlatform")
      .params(
        Map(
          "popId" -> popId,
          "genreId" -> genreId
        ))
      .asString
    if (result.code != 200) {
      error(s"request returned ${result.code} when downloading genre $genreId")
      ""
    } else {
      iosReadInDownloadedAppList(JsonParser(result.body)).toJson.prettyPrint
    }
  }

}
