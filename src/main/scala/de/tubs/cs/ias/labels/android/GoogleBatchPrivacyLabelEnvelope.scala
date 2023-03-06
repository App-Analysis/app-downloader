package de.tubs.cs.ias.labels.android

import de.tubs.cs.ias.util.googlebatch.{GoogleBatchEnvelope, Payload}
import spray.json.{JsArray, JsNull, JsNumber, JsString}

case class GoogleBatchPrivacyLabelPayload(appid: String) extends Payload {

  override def getPayloadJson: String = {
    JsArray(
      JsNull,
      JsNull,
      JsArray(
        JsArray(
          JsNumber(1),
          JsNumber(69),
          JsNumber(70),
          JsNumber(96),
          JsNumber(100),
          JsNumber(138)
        )
      ),
      JsNull,
      JsNull,
      JsArray(
        JsArray(
          JsString(appid),
          JsNumber(7)
        )
      )
    ).compactPrint
  }

}

case class GoogleBatchPrivacyLabelEnvelope(
    override val payload: GoogleBatchPrivacyLabelPayload)
    extends GoogleBatchEnvelope {
  override val endpoint: String = "Ws7gDc"
}
