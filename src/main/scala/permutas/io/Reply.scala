package permutas.io

import org.telegram.telegrambots.bots.{DefaultBotOptions, TelegramLongPollingBot}
import org.telegram.telegrambots.meta.api.methods.BotApiMethod
import org.telegram.telegrambots.meta.api.methods.send.{SendMessage, SendPhoto}
import org.telegram.telegrambots.meta.api.objects.Message

package object Reply {
  import permutas.constants._
  import Errors._

  def replyTextMessage(chatId: Long, replyTo: Integer, text: String): SendMessage = {
    val sendMessage = new SendMessage()
    sendMessage.enableMarkdown(true)
    sendMessage.setChatId(chatId)
    if (replyTo != null && replyTo > 0)
      sendMessage.setReplyToMessageId(replyTo)
    sendMessage.setText(text)
    return sendMessage
  }

  def replyTextMessage(m: Message, text: String): SendMessage = {
    replyTextMessage(m.getChatId, m.getMessageId, text)
  }

  def replyTextMessage(bot: TelegramLongPollingBot, chatId: Long, replyTo: Integer, text: String): Unit = {
    val msg = replyTextMessage(chatId, replyTo, text)
    bot.execute[Message, BotApiMethod[Message]](msg)
  }

  def replyError(m: Message, e: Errors.Value, lang: String): SendMessage = {
    println(s"Replying with error: ${e.toString}")
    replyTextMessage(m, e.toString())
  }
}
