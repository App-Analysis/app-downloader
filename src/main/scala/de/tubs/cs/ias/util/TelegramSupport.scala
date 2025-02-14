package de.tubs.cs.ias.util

import sttp.client4.quick._
import sttp.client4.Response
import wvlet.log.LogSupport

trait TelegramSupport {

  private var telegramClientContext : Option[String] = None

  protected def setContext(context : String) : Unit = telegramClientContext = Some(context)

  private def getContext : String = telegramClientContext match {
    case Some(value) => value
    case None => this.getClass.toString.split("\\.").last
  }

  def sendTelegramMessage(msg: String): Unit = {
    TelegramClient.send(getContext, msg)
  }

}

class TelegramClient(val token: String, val chat: Long) extends LogSupport {

  protected def sendMessage(msg: String): Unit = {
    try {
      val ps = Map("chat_id" -> chat, "text" -> msg)
      val response: Response[String] = quickRequest
        .get(uri"https://api.telegram.org/bot$token/sendMessage?$ps")
        .send()
      if (!response.is200)
        error(s"the response was ${response.code} saying ${response.body}")
    } catch {
      case e: Throwable => error(e.getMessage)
    }
  }

}

object TelegramClient extends LogSupport {

  private var vomSingleton: Option[TelegramClient] = None

  def withTelegramClientActive[T](token: String, chat: Long)(code: => T): T = {
    try {
      enableTelegramClient(token, chat)
      code
    } finally {
      disableTelegramClient()
    }
  }

  def enableTelegramClient(token: String, chat: Long): Unit = synchronized {
    vomSingleton match {
      case Some(_) =>
      case None    => vomSingleton = Some(new TelegramClient(token, chat))
    }
  }

  def disableTelegramClient(): Unit = synchronized {
    vomSingleton = None
  }

  def send(context: String, msg: String): Unit = synchronized {
    vomSingleton match {
      case Some(vom) => vom.sendMessage(s"[$context] $msg")
      case None      =>
    }
  }

}
