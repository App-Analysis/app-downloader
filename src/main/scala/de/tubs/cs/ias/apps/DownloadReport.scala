package de.tubs.cs.ias.apps

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

case class DownloadReport(appName: String,
                          appBundleId: String,
                          appVer: String,
                          appId: Int,
                          appVerId: Int,
                          downloadedIPA: String,
                          downloadedVerId: Int)

object DownloadReportReader extends DefaultJsonProtocol {

  implicit val downloadReportFormat: RootJsonFormat[DownloadReport] =
    jsonFormat7(DownloadReport.apply)

}
