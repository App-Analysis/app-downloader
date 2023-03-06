package de.tubs.cs.ias.util.googlebatch

import spray.json.{JsArray, JsNull, JsString, JsValue}

trait Payload {

  def getPayloadJson: String

}

trait GoogleBatchEnvelope {

  val endpoint: String
  val payload: Payload

  def getJson(order: String = "1"): JsValue = {
    JsArray(
      JsString(endpoint),
      JsString(payload.getPayloadJson),
      JsNull,
      JsString(order)
    )
  }

}

object GoogleBatchPayload {

  def createBodyPayload(requests: List[GoogleBatchEnvelope]): String = {
    var counter = -1
    JsArray(
      JsArray(
        requests.map { elem =>
          counter = counter + 1
          elem.getJson(counter.toString)
        }.toVector
      )
    ).compactPrint
  }

}
