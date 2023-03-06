package de.tubs.cs.ias.applist

import spray.json._
import wvlet.log.LogSupport

object AppListParser extends DefaultJsonProtocol with LogSupport {

  implicit val mobileAppFormat: RootJsonFormat[MobileApp] = jsonFormat5(
    MobileApp)

  implicit val appListFormat: RootJsonFormat[MobileAppList] = jsonFormat2(
    MobileAppList)

}
