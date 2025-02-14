package de.tubs.cs.ias.util

import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import spray.json._

object Config extends DefaultJsonProtocol {

  implicit val appCategoryFormat: RootJsonFormat[AppCategory] = jsonFormat2(
    AppCategory.apply)

  implicit val loginConfigFormat: RootJsonFormat[LoginConfig] = jsonFormat3(
    LoginConfig.apply)

  implicit val telegramConfigFormat: RootJsonFormat[TelegramConfig] =
    jsonFormat3(TelegramConfig.apply)

  implicit val androidConfigFormat: RootJsonFormat[AndroidConfig] =
    jsonFormat3(AndroidConfig.apply)

  implicit val configFormat: RootJsonFormat[Config] = jsonFormat4(Config.apply)

  def read(path: String): Config = {
    fsi.readInTextFile(path).parseJson.convertTo[Config]
  }

}

case class AppCategory(name: String, id: String)

case class LoginConfig(email: String, password: String, twoFA: String)

case class TelegramConfig(enable: Boolean, apiKey: String, chatId: String)

case class AndroidConfig(categories: List[AppCategory],
                         osFolderName: String,
                         googleplay: String)

case class Config(android: AndroidConfig,
                  telegram: TelegramConfig,
                  maxAmountApps: Int,
                  downloadFolderRoot: String)
