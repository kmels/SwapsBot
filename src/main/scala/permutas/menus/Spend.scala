package permutas.menus

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import MenuUtils._

import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup

import permutas.constants._
import permutas.Types._

package object Spend {

  def spendable_coins(uid: Int)(implicit wm: WallMap): Seq[String] = {
    val options: ListBuffer[String] = ListBuffer()
    for (w <- wm.getOrElse(uid, mutable.Map())) {
      val balance = w._2.getBalance
      val network = w._2.getParams
      val minBalance = network.getMinNonDustOutput.add(network.getReferenceDefaultMinTxFee)

      if (balance.isGreaterThan(minBalance)) {
        options += w._1
      }
    }
    options
  }

  def get_spend_coin_menu_keyboard(uid: Int, lang: String)(implicit wm: WallMap): ReplyKeyboardMarkup = {
    val options: ListBuffer[String] = ListBuffer()
    options ++= spendable_coins(uid)
    options += getCommand(Commands.BACK, lang)
    make_options_keyboard(options.toList, 2)
  }
}