package permutas.menus

import scala.collection.mutable.ListBuffer

package object Settings {
  import MenuUtils._
  import permutas.constants.Commands
  import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard
  import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup

  def get_settings_menu_keyboard(uid: Int, lang: String): ReplyKeyboardMarkup = {
    val options: ListBuffer[String] = ListBuffer()
    options += getCommand(Commands.WHOAMI,lang)
    options += getCommand(Commands.SEED,lang)
    options += getCommand(Commands.LANGUAGE,lang)
    options += getCommand(Commands.INFO,lang)
    options += getCommand(Commands.BACK,lang)
    make_options_keyboard(options.toList, 2)
  }
}