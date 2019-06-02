package permutas.menus

import org.bitcoinj.kits.BIP47AppKit
import org.bitcoinj.core._
import org.bitcoinj.core.bip47._
import org.bitcoinj.script._
import org.telegram.telegrambots.meta.api.objects.Message
import org.telegram.telegrambots.meta.api.methods.send.SendMessage

import permutas.wallets._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

package object Wallets {
  import MenuUtils._
  import permutas.io.Reply._
  import permutas.MongoDb._
  import permutas.Types._
  import permutas.constants._
  import CheckedExceptions._
  import States._
  import Spend._

  import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard
  import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup

  import permutas.io.Pretty._

  def get_wallet_menu_keyboard(lang: String): ReplyKeyboardMarkup = {

    val options =
      List(
        Commands.WALLETS,
        Commands.ADDRESS_LIST,
        Commands.LOCK_SWAP,
        Commands.TRANSACTIONS,
        Commands.SPEND,
        Commands.LOCK,
        Commands.PEERS,
        Commands.BACK
      ).map(command => getCommand(command, lang))

    make_options_keyboard(options, 2)
  }

  def viewWallets(m: Message, lang: String)(implicit wm: WallMap): SendMessage = {
    save_state(m, WALLETMENU)
    val wallets: mutable.Map[String, BIP47AppKit] = wm.getOrElse(m.getFrom.getId, mutable.Map())
    var txt: String = "Wallets:\n"

    // Payment address
    val pcodes: mutable.Set[String] = mutable.Set()
    for (elem <- wallets) {
      pcodes += elem._2.getPaymentCode
    }
    println(s"PCODES Size: ${pcodes}")
    assert(pcodes.size == 1)

    txt += s"*Universal address:* ${pcodes.head}\n\n"
    txt += "Balances\n"
    for (elem <- wallets) {
      val height = elem._2.getvWallet().getLastBlockSeenHeight
      txt = txt + s"\t${formatBalance(elem._2)} @ ${height}\n"
    }

    sendChooseOptionMessage(m, get_wallet_menu_keyboard(lang), lang, txt)
  }

  def watchLimitSwap(kit: BIP47AppKit,
                     channel: BIP47Channel,
                     wantSecretHash: Sha256Hash): Unit = {
    println(s"Watching HTLC ... ${channel}")
    val swap = new Swap(kit.getParams)

    println(s"Theirs pyment code: ${channel.getPaymentCode}")

    val refundPubkeys = channel.getOutgoingAddresses
    println(s"Pubkeys: ${refundPubkeys.size()}")

    val swapPubKey = new BIP47PaymentCode(kit.getPaymentCode).derivePubKeyAt(kit.getParams, 0)

    // Alices (hash's generator) refund pubkey
    // TODO: Refund's pubkey is current incoming address pubkey
    val refundPubKey =
    new BIP47Account(kit.getParams, channel.getPaymentCode).keyAt(0).getPubKey

    val redeemScript = swap.getRedeemScript(wantSecretHash,
      refundPubKey, swapPubKey, Swaps.SwapUtils.REFUND_TIME_NEEDED)

    val validP2SH = ScriptPattern.isPayToScriptHash(redeemScript)

    val p2shAddress: Address =
      ScriptBuilder.createP2SHOutputScript(redeemScript)
        .getToAddress(kit.getParams)

    println(s"Adding watch script: ${p2shAddress}")
    kit.getvWallet().addWatchedScripts(List(redeemScript))

    println(s"Adding watch: ${p2shAddress}")

    kit.getvWallet().addWatchedAddress(p2shAddress)

    println("TODO: Rescanning blockchain...")
    }

  def messageOnWallets(m: Message, lang: String)(implicit wm: WallMap): SendMessage = if (m.hasText) {
    val walletsCmd = getCommand(Commands.WALLETS, lang)
    val txsCmd = getCommand(Commands.TRANSACTIONS, lang)
    val spendCmd = getCommand(Commands.SPEND, lang)
    val lockCmd = getCommand(Commands.LOCK, lang)
    val addrCmd = getCommand(Commands.ADDRESS_LIST, lang)
    val peersCmd = getCommand(Commands.PEERS, lang)

    m.getText match {

      case `addrCmd` => {
        save_state(m, ADDRESS_LIST)
        val keyboard = get_all_coins_menu_keyboard(m.getFrom.getId, lang)
        sendChooseOptionMessage(m, keyboard, lang)
      }

      case `walletsCmd` => viewWallets(m, lang)

      case `txsCmd` => {
        var reply = "Transactions:\n"

        val wallets: UserWallets = wm.getOrElse(m.getFrom.getId, mutable.Map())
        for (elem <- wallets) {
          val coin = elem._2.getCoinName
          reply += s"*${elem._2.getCoinName}*\n"

          val wallet = elem._2.getvWallet()

          for (tx <- wallet.getTransactions(false)) {
            val outputs = tx.getWalletOutputs(wallet)
            val outputAddresses = outputs.map(out =>
              out.getAddressFromP2PKHScript(wallet.getParams).toBase58)

            reply += "\n------------------------\n"
            reply += s"Hash: ${getTransactionLink(elem._2.getCoinName, tx)}\n"

            if (!tx.isPending) {
              reply += s"Confidence: ${tx.getConfidence}\n"
            } else if (tx.getConfidence.getSource == TransactionConfidence.Source.SELF) {
              println(s"removing tx: ${tx}")
              elem._2.unsafeRemoveTxHash(tx.getHash)
            } else {
              reply += s"Confidence: ${tx.getConfidence}\n"
            }
            //if (tx.getConfidence.getLastBroadcastedAt!=null)
            //  reply += s"Broadcast date: ${tx.getConfidence.getLastBroadcastedAt}\n"
            if (tx.isTimeLocked)
              reply += s"Time lock: ${tx.getLockTime}\n"

            val spent = tx.getValueSentFromMe(wallet)
            if (spent.isPositive)
              reply += s"Value sent from me: ${formatAmount(spent, elem._2.getCoinName)}\n"

            val received = tx.getValueSentToMe(wallet)
            if (received.isPositive)
              reply += s"Value sent to me: ${formatAmount(received, elem._2.getCoinName)}\n"

            reply += s"Output addresses: ${outputAddresses.mkString(",")}"

          }
        }

        replyTextMessage(m, reply)
      }

      case `spendCmd` => {
        save_state(m, SPEND)

        val keyboard = get_spend_coin_menu_keyboard(m.getFrom.getId, lang)
        if (spendable_coins(m.getFrom.getId).size > 0)
          sendChooseOptionMessage(m, keyboard, lang)
        else throw new CatchError(Errors.MISSING_SPEND_COINS)
      }

      case `lockCmd` => {
        save_state(m, LOCK)
        sendChooseOptionMessage(m, get_spend_coin_menu_keyboard(m.getFrom.getId, lang), lang)
      }

      case `peersCmd` => {
        var reply = "*Connections:*\n"
        val wallets: UserWallets = wm.getOrElse(m.getFrom.getId, mutable.Map())
        for (elem <- wallets) {
          val coin = elem._2.getCoinName
          reply += s"*${coin}*: "
          reply += s"${elem._2.getConnectedPeers.size} peers\n"
        }

        save_state(m, WALLET_PEERS)
        replyTextMessage(m, reply)
      }
    }
  } else {
    sendChooseOptionMessage(m, Start.get_main_menu_keyboard(lang), lang)
  }

}
