package permutas.menus

import java.util

import org.telegram.telegrambots.meta.api.objects.Message
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard
import org.telegram.telegrambots.meta.api.methods.send.SendMessage
import org.telegram.telegrambots.meta.api.objects.replykeyboard.{InlineKeyboardMarkup, ReplyKeyboardMarkup}
import org.telegram.telegrambots.meta.exceptions.TelegramApiException
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.InlineKeyboardButton

import scala.collection.mutable.ListBuffer

package object MenuUtils {
  import permutas.Types._
  import permutas.MongoDb._
  import permutas.constants._
  import States._

  import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup
  import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow

  import permutas.i18n.translations

  def make_inline_keyboard
  (buttonRows: List[List[(Labels.Value, Either[String, Callbacks.Value])]],
   lang: String): InlineKeyboardMarkup = {

    val markupInline = new InlineKeyboardMarkup
    val rows = new util.ArrayList[util.List[InlineKeyboardButton]]

    for (buttonRow <- buttonRows) {
      val inlineRow = new util.ArrayList[InlineKeyboardButton]
      for (button <- buttonRow) {

        val keyboardButton =
          new InlineKeyboardButton()
            .setText(translations((button._1.toString, lang)))

        if (button._2.isRight) {

          keyboardButton
            .setCallbackData(button._2.right.get.toString)
        } else {
          val call = button._2.left.get
          if (call.startsWith("http"))
            keyboardButton.setUrl(call)
          else
            keyboardButton.setCallbackData(button._2.left.get)
        }

        inlineRow.add(keyboardButton)
      }
      rows.add(inlineRow)
    }
    markupInline.setKeyboard(rows)
    return markupInline
  }

  def get_all_coins_menu_keyboard(uid: Int, lang: String)(implicit wm: WallMap): ReplyKeyboardMarkup = {
    val options: ListBuffer[String] = ListBuffer()
    val coins: List[String] = wm.get(uid).fold[List[String]](List())(map => map.keys.toList)
    options ++= coins
    options += getCommand(Commands.BACK,lang)
    make_options_keyboard(options.toList, 3)
  }

  def make_options_keyboard(xs: List[String], ncols: Int): ReplyKeyboardMarkup = {
    //println(s"Making options: ${xs.mkString(",")} in ${ncols} columns")
    val replyKeyboardMarkup = new ReplyKeyboardMarkup
    replyKeyboardMarkup.setSelective(true)
    replyKeyboardMarkup.setResizeKeyboard(true)
    replyKeyboardMarkup.setOneTimeKeyboard(false)

    val keyboard: java.util.List[KeyboardRow] = new java.util.ArrayList()
    var nextRow: KeyboardRow = null

    for (opt <- xs.zipWithIndex) {
      if (opt._2 % ncols == 0) {
        nextRow = new KeyboardRow
        keyboard.add(nextRow)
      }

      nextRow.add(opt._1)
    }

    replyKeyboardMarkup.setKeyboard(keyboard)
    return replyKeyboardMarkup
  }

  def getCommand(command: Commands.Value, lang:String) =
      translations((command.toString, lang))

  def getHelpMessage(lang: String): String = ""

  def sendHelpMessage(chatId: Long, messageId: Integer, markup: ReplyKeyboardMarkup, lang: String): SendMessage = {
    val sendMessage = new SendMessage()
    sendMessage.enableMarkdown(true)
    sendMessage.setChatId(chatId)
    sendMessage.setReplyToMessageId(messageId)
    sendMessage.setReplyMarkup(markup)
    sendMessage.setText(getHelpMessage(lang))
    return sendMessage
  }

  def sendMessageDefault(m: Message, lang: String): SendMessage = {
    val markup = Start.get_main_menu_keyboard(lang)
    save_state(m, MAINMENU)
    return sendHelpMessage(m.getChatId, m.getMessageId, markup, lang)
  }

  def sendChooseOptionMessage(m: Message,
                              replyKeyboard: ReplyKeyboard, lang: String,
                              text: String = "Choose option"
                             ): SendMessage = {
    //println(s"sendChooseOptionMessage: ${text}")
    val sendMessage: SendMessage = new SendMessage()
    sendMessage.enableMarkdown(true)
    sendMessage.setChatId(m.getChatId.toString)
    sendMessage.setReplyToMessageId(m.getMessageId)
    sendMessage.setReplyMarkup(replyKeyboard)
    sendMessage.setText(text)
    return sendMessage
  }
}