package de.tubs.cs.ias.applist.android

import de.tubs.cs.ias.applist.{MobileApp, MobileAppList}
import spray.json.{JsArray, JsNull, JsNumber, JsString, JsValue, JsonParser}

object AppListParser {

  def readInTopChartEntry(entry: JsArray, position: Int): MobileApp = {
    val appId = entry.elements.head
      .asInstanceOf[JsArray]
      .elements
      .head
      .asInstanceOf[JsArray]
      .elements
      .head
      .asInstanceOf[JsString]
      .value
    val name = entry.elements.head
      .asInstanceOf[JsArray]
      .elements
      .apply(3)
      .asInstanceOf[JsString]
      .value
    val category = entry.elements.head
      .asInstanceOf[JsArray]
      .elements
      .apply(5)
      .asInstanceOf[JsString]
      .value
    val price =
      entry.elements.head.asInstanceOf[JsArray].elements.apply(8) match {
        case JsNull => "0 EUR"
        case price: JsArray =>
          val priceElements = price.elements
            .apply(1)
            .asInstanceOf[JsArray]
            .elements
            .head
            .asInstanceOf[JsArray]
            .elements
          s"${priceElements.head.asInstanceOf[JsNumber].value} ${priceElements.apply(1).asInstanceOf[JsString].value}"
      }
    MobileApp(name, appId, category, position, price)
  }

  def readInTopChartPayload(payload: JsArray): MobileAppList = {
    val sub = payload.elements.head
      .asInstanceOf[JsArray]
      .elements
      .apply(1)
      .asInstanceOf[JsArray]
      .elements
      .head
      .asInstanceOf[JsArray]
    assert(sub.elements.length == 29)
    assert(sub.elements.apply(3).asInstanceOf[JsArray].elements.length == 1)
    assert(sub.elements.count(_.equals(JsNull)) == 27)
    val entries = sub.elements
      .apply(28)
      .asInstanceOf[JsArray]
      .elements
      .head
      .asInstanceOf[JsArray]
    var counter = 0
    val apps = entries.elements.map {
      case elem: JsArray =>
        counter = counter + 1
        readInTopChartEntry(elem, counter)
    }
    val category: Set[String] =
      apps.map(_.category).map(elem => elem -> elem).toList.toMap.keySet
    MobileAppList(
      apps.toList,
      if (category.size != 1) "MIXED" else category.head
    )
  }

  def readInAppBatchResponse(response: JsValue): MobileAppList = {
    response match {
      case outerEnvelope: JsArray =>
        assert(
          outerEnvelope.elements.length == 3,
          s"out element should have 3 and not ${outerEnvelope.elements.length} elements")
        outerEnvelope.elements.head match {
          case payload: JsArray =>
            assert(
              payload.elements.length == 7,
              s"payload element should have 7 and not ${payload.elements.length} elements")
            payload.elements.apply(2) match {
              case content: JsString =>
                readInTopChartPayload(
                  JsonParser(content.value).asInstanceOf[JsArray])
              case x: JsValue =>
                throw new RuntimeException(
                  s"expected JsString as payload content and not ${x.getClass}")
            }
          case x: JsValue =>
            throw new RuntimeException(
              s"expected JsArray for payload and not ${x.getClass}")
        }
      case x: JsValue =>
        throw new RuntimeException(
          s"expected JsArray for outer envelope and not ${x.getClass}")
    }
  }

}
