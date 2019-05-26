package permutas.menus

import org.telegram.telegrambots.meta.api.methods.send.SendMessage
import org.telegram.telegrambots.meta.api.objects.Message
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup

package object Start {

  import permutas.MongoDb._
  import permutas.menus.MenuUtils._
  import permutas.constants._
  import permutas.Types._
  import CheckedExceptions._

  def get_main_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options =
      List(
        Commands.WALLETS,
        Commands.LOCK_SWAP,
        Commands.REQUEST,
        Commands.SPEND,
        Commands.HTLC_LIST,
        Commands.SETTINGS
      ).map(command => getCommand(Commands.withName(command.toString), lang))

    make_options_keyboard(options, 3)
  }

  def getCommand(command: Commands.Value, lang:String) =
    permutas.i18n.translations((command.toString, lang))

  def messageOnMainMenu(m: Message, lang: String)(implicit wm: WallMap): SendMessage = if (m.hasText) {
    println(s"messageOnMainMenu: ${m.getText}")
    val walletsCmd = getCommand(Commands.WALLETS, lang)
    val swapsCmd = getCommand(Commands.HTLC_LIST, lang)
    val spendCmd = getCommand(Commands.SPEND, lang)
    val lockCmd = getCommand(Commands.LOCK, lang)
    val settingsCmd = getCommand(Commands.SETTINGS, lang)

    m.getText() match {

      case `walletsCmd` => Wallets.viewWallets(m, lang)

      case `swapsCmd` => Swaps.viewSwaps(m, lang)

      case `spendCmd` => {
        save_state(m, States.SPEND)

        val keyboard = Spend.get_spend_coin_menu_keyboard(m.getFrom.getId, lang)
        if (Spend.spendable_coins(m.getFrom.getId).size > 0)
          sendChooseOptionMessage(m, keyboard, lang)
        else throw new CatchError(Errors.MISSING_SPEND_COINS)
      }

      case `lockCmd` => {
        save_state(m, States.LOCK)
        sendChooseOptionMessage(m, Spend.get_spend_coin_menu_keyboard(m.getFrom.getId, lang), lang)
      }

      case `settingsCmd` => {
        save_state(m, States.SETTINGSMENU)
        sendChooseOptionMessage(m,
          Settings.get_settings_menu_keyboard(m.getFrom.getId, lang), lang)
      }

      case _ => sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
    }
  } else {
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }
}