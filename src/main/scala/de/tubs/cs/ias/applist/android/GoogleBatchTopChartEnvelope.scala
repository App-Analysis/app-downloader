package de.tubs.cs.ias.applist.android

import de.tubs.cs.ias.util.googlebatch.{GoogleBatchEnvelope, Payload}
import spray.json.{JsArray, JsNull, JsNumber, JsString}

case class GoogleBatchTopChartPayload(chart: String,
                                      category: String,
                                      count: Int)
    extends Payload {

  override def getPayloadJson: String = {
    JsArray(
      JsArray(
        JsNull,
        JsArray(JsArray(JsNull, JsArray(JsNull, JsNumber(count))),
                JsNull,
                JsNull,
                JsArray(JsNumber(113))),
        JsArray(JsNumber(2), JsString(category), JsString(chart))
      )).compactPrint
  }

}

case class GoogleBatchTopChartEnvelope(
    override val payload: GoogleBatchTopChartPayload)
    extends GoogleBatchEnvelope {
  override val endpoint = "vyAe2"
}
