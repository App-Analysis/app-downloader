package de.tubs.cs.ias.applist.ios

import de.tubs.cs.ias.applist.{MobileApp, MobileAppList}
import spray.json
import spray.json.{JsArray, JsString, JsValue}

object AppListParser {

  def readInAppList(value: JsValue): MobileAppList = {
    val genre = value.asJsObject
      .fields("pageData")
      .asJsObject
      .fields("segmentedControl")
      .asJsObject
      .fields("segments")
      .asInstanceOf[JsArray]
      .elements
      .head
      .asJsObject
      .fields("pageData")
      .asJsObject
      .fields("genre")
      .asJsObject
      .fields("name")
      .asInstanceOf[JsString]
      .value
    var counter = 0
    val apps = value.asJsObject
      .fields("pageData")
      .asJsObject
      .fields("segmentedControl")
      .asJsObject
      .fields("segments")
      .asInstanceOf[json.JsArray]
      .elements
      .head
      .asJsObject
      .fields("pageData")
      .asJsObject
      .fields("selectedChart")
      .asJsObject
      .fields("adamIds")
      .asInstanceOf[JsArray]
      .elements
      .toList
      .map { id =>
        MobileApp("UNKNOWN", id.asInstanceOf[JsString].value, genre, {
          counter = counter + 1
          counter
        }, "0 €")
      }
    MobileAppList(apps, genre)
  }
}
