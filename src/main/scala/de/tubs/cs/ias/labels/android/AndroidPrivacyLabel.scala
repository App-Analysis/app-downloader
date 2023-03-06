/**
  *  All glory to: Benjamin Altpeter @ https://github.com/baltpeter/parse-play
  */
package de.tubs.cs.ias.labels.android

import spray.json.{
  DefaultJsonProtocol,
  JsArray,
  JsBoolean,
  JsNull,
  JsString,
  JsValue,
  RootJsonFormat,
  enrichAny
}

import scala.annotation.tailrec
case class SecurityPractices(dataEncryptedInTransit: Option[Boolean],
                             canRequestDataDeletion: Option[Boolean],
                             committedToPlayFamilyPolicy: Option[Boolean],
                             independentReview: Option[Boolean])

case class DataEntry(category: String,
                     ttype: String,
                     purposes: String,
                     optional: Boolean)

case class DataSection(entries: List[DataEntry])

case class DeveloperInfo(name: String,
                         path: String,
                         website: String,
                         email: String,
                         address: String)

case class AndroidPrivacyLabel(name: String,
                               appId: String,
                               developer: DeveloperInfo,
                               icon: String,
                               privacyPolicy: String,
                               sharedData: DataSection,
                               collectedData: DataSection,
                               securityPractices: SecurityPractices)

object AndroidPrivacyLabel extends DefaultJsonProtocol {

  private def StringConverter(context: String)(value: JsValue): String = {
    value match {
      case JsString(value) => value
      case _ =>
        throw new RuntimeException(
          value.prettyPrint + " is not of type JsString - " + context)
    }
  }

  @tailrec
  private def downTheRabbitHole[T](rabbit: JsValue,
                                   hole: List[Int],
                                   converter: JsValue => T,
                                   optional: Option[T] = None): T = {
    if (hole.isEmpty) {
      converter(rabbit)
    } else {
      rabbit match {
        case JsArray(elements) =>
          downTheRabbitHole(elements(hole.head), hole.tail, converter)
        case _ if optional.nonEmpty =>
          optional.get
        case x if optional.isEmpty =>
          throw new RuntimeException(x.prettyPrint + " is not of type JsArray")
      }
    }
  }

  def parseSecurityPractices(value: JsValue): SecurityPractices = {
    def checkHeadings(headings: List[String],
                      positive: String,
                      negative: String): Option[Boolean] = {
      val pos = headings.contains(positive)
      val neg = headings.contains(negative)
      (pos, neg) match {
        case (true, true) =>
          throw new RuntimeException(
            "cannot be both " + positive + " and " + negative)
        case (false, false) => None
        case (true, false)  => Some(true)
        case (false, true)  => Some(false)
      }
    }
    value match {
      case JsArray(practices) =>
        val headings = practices.map {
          case JsArray(practice) =>
            practice(1).asInstanceOf[JsString].value
          case x =>
            throw new RuntimeException(
              x.prettyPrint + " is not of type JsArray")
        }.toList
        SecurityPractices(
          checkHeadings(headings,
                        "Data is encrypted in transit",
                        "Data isn't encrypted"),
          checkHeadings(headings,
                        "You can request that data be deleted",
                        "Data can't be deleted"),
          checkHeadings(headings,
                        "Committed to follow the Play Families Policy",
                        "No Negative"),
          checkHeadings(headings, "Independent security review", "No Negative"),
        )
      case JsNull => SecurityPractices(None, None, None, None)
      case _ =>
        throw new RuntimeException(
          value.prettyPrint + " is neither JsNull nor an array")
    }
  }

  def parseCategory(arr: JsArray): (String, String) = {
    (downTheRabbitHole(arr, List(1), StringConverter("category name")),
     downTheRabbitHole(arr,
                       List(2, 1),
                       StringConverter("category description")))

  }

  def parseDataSection(arr: JsValue, idx: Int): DataSection = {
    arr match {
      case JsNull => DataSection(List())
      case x: JsArray =>
        downTheRabbitHole(x, List(idx, 0), _.asInstanceOf[JsArray]) match {
          case idx0: JsArray =>
            val ret = idx0.elements.flatMap {
              case r: JsArray =>
                val (name, _) =
                  parseCategory(r.elements.head.asInstanceOf[JsArray])
                r.elements(4).asInstanceOf[JsArray].elements.map { d =>
                  DataEntry(
                    name,
                    downTheRabbitHole(d,
                                      List(0),
                                      StringConverter("entry type")),
                    downTheRabbitHole(d,
                                      List(2),
                                      StringConverter("entry purposes")),
                    downTheRabbitHole(d,
                                      List(1),
                                      _.asInstanceOf[JsBoolean].value)
                  )
                }
            }
            DataSection(ret.toList)
          case x =>
            throw new RuntimeException(
              s"data section element $idx,0 is not JsArray: ${x.prettyPrint}")
        }
    }
  }

  def apply(payload: JsArray): AndroidPrivacyLabel = {
    val data =
      downTheRabbitHole(payload, List(1, 2), elem => elem.asInstanceOf[JsArray])
    val name: String =
      downTheRabbitHole(data, List(0, 0), StringConverter("name"))
    val appId: String =
      downTheRabbitHole(payload, List(1, 11, 0, 0), StringConverter("appid"))
    val developer = DeveloperInfo(
      downTheRabbitHole(data, List(68, 0), StringConverter("dev name")),
      downTheRabbitHole(data, List(68, 1, 4, 2), StringConverter("dev path")),
      downTheRabbitHole(data,
                        List(69, 0, 5, 2),
                        StringConverter("dev url"),
                        Some("")),
      downTheRabbitHole(data, List(69, 1, 0), StringConverter("dev email")),
      downTheRabbitHole(data,
                        List(69, 2, 0),
                        StringConverter("dev addr"),
                        Some(""))
    )
    val icon =
      downTheRabbitHole(data, List(95, 0, 3, 2), StringConverter("icon"))
    val privacyPolicy = downTheRabbitHole(data,
                                          List(99, 0, 5, 2),
                                          StringConverter("policy"),
                                          Some(""))
    val shared = parseDataSection(
      downTheRabbitHole(data, List(137, 4), identity, Some(JsNull)),
      0)
    val collected = parseDataSection(
      downTheRabbitHole(data, List(137, 4), identity, Some(JsNull)),
      1)
    val securityPractices = parseSecurityPractices(
      downTheRabbitHole(data, List(137, 9, 2), identity, Some(JsNull)))
    AndroidPrivacyLabel(
      name,
      appId,
      developer,
      icon,
      privacyPolicy,
      shared,
      collected,
      securityPractices
    )
  }

  implicit val securityPracticesFormat: RootJsonFormat[SecurityPractices] =
    jsonFormat4(SecurityPractices)

  implicit val dataEntryFormat: RootJsonFormat[DataEntry] = jsonFormat4(
    DataEntry)

  implicit val dataSectionFormat: RootJsonFormat[DataSection] = jsonFormat1(
    DataSection)

  implicit val developerInfoFormat: RootJsonFormat[DeveloperInfo] = jsonFormat5(
    DeveloperInfo)

  implicit val androidPrivacyLabelFormat: RootJsonFormat[AndroidPrivacyLabel] =
    jsonFormat8(AndroidPrivacyLabel.apply)

  def prettyPrint(pl: AndroidPrivacyLabel): String = {
    pl.toJson.prettyPrint
  }

}
