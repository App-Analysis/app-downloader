package de.tubs.cs.ias.util

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

case class ActionReport(success: Int, failure: Int, fails: Map[String, String])

case class ActionReportCollection(collection: Map[String, ActionReport])

object ActionReportJsonWriter extends DefaultJsonProtocol {

  implicit val actionReportFormat: RootJsonFormat[ActionReport] = jsonFormat3(
    ActionReport)

  implicit val actionReportCollection: RootJsonFormat[ActionReportCollection] =
    jsonFormat1(ActionReportCollection)

}
