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
import wvlet.log.LogSupport

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

object AndroidPrivacyLabel extends DefaultJsonProtocol with LogSupport {

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
    //println(hole.mkString(","))
    if (hole.isEmpty) {
      converter(rabbit)
    } else {
      rabbit match {
        case JsArray(elements) =>
          assert(elements.length > hole.head,
                 s"no ${hole.head} in ${JsArray(elements).prettyPrint}")
          downTheRabbitHole(elements(hole.head), hole.tail, converter, optional)
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
      case JsNull     => DataSection(List())
      case x: JsArray =>
        //enumerate(x)
        downTheRabbitHole(x, List(idx, 0), identity) match {
          case JsNull =>
            DataSection(List())
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

  def enumerate(arr: JsArray,
                depth: Int = 1,
                counters: List[Int] = List(),
                sb: StringBuilder = new StringBuilder()): StringBuilder = {
    var counter = 0
    def getCounter: String = {
      (counter :: counters).reverse.mkString("-")
    }
    val formatCmd = s"%${depth}s"
    val formatString = String.format(formatCmd, "")
    arr.elements.foreach {
      case elem: JsString =>
        sb.append(s"$formatString $getCounter : ${elem.value}\n")
        counter = counter + 1
      case elem: JsArray =>
        sb.append(s"$formatString $getCounter : [\n")
        enumerate(elem, depth + 1, counter :: counters, sb = sb)
        sb.append(s"$formatString ]\n")
        counter = counter + 1
      case other: JsValue =>
        sb.append(s"$formatString $getCounter : ${other.prettyPrint}\n")
        counter = counter + 1
    }
    sb
  }

  def apply(payload: JsArray): AndroidPrivacyLabel = {
    var current = "data"
    try {
      val data =
        downTheRabbitHole(payload,
                          List(1, 2),
                          elem => elem.asInstanceOf[JsArray])
      current = "name"
      val name: String =
        downTheRabbitHole(data, List(0, 0), StringConverter("name"))
      current = "appId"
      val appId: String =
        downTheRabbitHole(payload, List(1, 11, 0, 0), StringConverter("appid"))
      current = "developer"
      val developer = DeveloperInfo(
        downTheRabbitHole(data, List(68, 0), StringConverter("dev name")),
        downTheRabbitHole(data, List(68, 1, 4, 2), StringConverter("dev path")),
        downTheRabbitHole(data,
                          List(69, 0, 5, 2),
                          StringConverter("dev url"),
                          Some("")),
        downTheRabbitHole(data, List(69, 1, 0), StringConverter("dev email")),
        "N/A (dropped in current label info)"
        /*downTheRabbitHole(data,
                        List(69, 2, 0),
                        StringConverter("dev addr"),
                        Some(""))*/
      )
      current = "icon"
      val icon = {
        downTheRabbitHole(data, List(95, 0, 3, 2), StringConverter("icon"))
      }
      current = "privacy policy"
      val privacyPolicy = downTheRabbitHole(data,
                                            List(99, 0, 5, 2),
                                            StringConverter("policy"),
                                            Some(""))
      current = "shared"
      val shared = parseDataSection(
        downTheRabbitHole(data, List(137, 4), identity, Some(JsNull)),
        0)
      current = "collected"
      val collected = parseDataSection(
        downTheRabbitHole(data, List(137, 4), identity, Some(JsNull)),
        1)
      current = "security practices"
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
    } catch {
      case x: Throwable =>
        error(s"unable to retrieve $current")
        error(enumerate(payload).mkString)
        throw x
    }
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
