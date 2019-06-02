package permutas.menus

import java.math.BigInteger

import org.bitcoinj.core._
import org.bitcoinj.core.bip47.{BIP47Account, BIP47Channel, BIP47PaymentCode}
import org.bitcoinj.kits.BIP47AppKit
//import org.bitcoinj.params._
import org.bitcoinj.script.{Script, ScriptBuilder, ScriptChunk, ScriptPattern}
import org.bitcoinj.script.ScriptOpCodes._

import org.telegram.telegrambots.meta.api.objects.Message
import org.telegram.telegrambots.meta.api.methods.send.SendMessage

import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup

import org.telegram.telegrambots.meta.api.objects.replykeyboard.{InlineKeyboardMarkup, ReplyKeyboardMarkup}

import permutas.MongoDb._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

package object Swaps {
  import MenuUtils._
  import permutas.Types._
  import permutas.constants._
  import permutas.i18n.translations
  import permutas.io.Pretty._

  import scala.collection.JavaConverters._
  import scala.collection.JavaConversions._

  def get_limit_swap_inline_keyboard(lang: String): InlineKeyboardMarkup = {
    val opts = List((Labels.SWAP_FILL, Right(Callbacks.SWAP_LIMIT_FILL)))
    return row_msg_keyboard(opts, lang)
  }

  object SwapUtils {

    val REFUND_TIME_NEEDED = 2; //24*60 //2;

    def maybeLimitSwap(data: Array[Byte]): Option[LimitSwap] = {
      if (data.size != 48)
        return None

      val coinBytes = data.take(4)
      val coinStartByte = if (coinBytes(0) == '-'.toByte) 1 else 0
      val wantCoin = new String(coinBytes.drop(coinStartByte))
      var tail = data.drop(4)

      val priceBytes = tail.take(4)
      val priceBuff = java.nio.ByteBuffer.wrap(priceBytes)
      tail = tail.drop(4)

      val locktimeBytes = tail.take(8)
      val locktimeBuff = java.nio.ByteBuffer.wrap(locktimeBytes)
      tail = tail.drop(8)

      val secretHash = new Sha256Hash(tail.take(32))
      tail = tail.drop(32)

      assert(tail.size == 0, "Tail is size: ${tail.size}, should be 0!")
      return Some(wantCoin, priceBuff.getFloat, locktimeBuff.getLong, secretHash)
    }

    def maybeRedeemableSwap(script: Script): Option[RedeemableSwap] = {
      val chunks = script.getChunks
      if (chunks.size() != 13)
        return None
      if (!chunks.get(0).equalsOpCode(OP_IF))
        return None
      if (!chunks.get(1).equalsOpCode(OP_SHA256))
        return None
      if (!chunks.get(2).isPushData)
        return None
      val hash: Sha256Hash = new Sha256Hash(chunks.get(2).data)
      if (!chunks.get(4).isPushData)
        return None
      if (!chunks.get(7).isPushData) {
        return None
      }
      var locktimeBytes = chunks.get(7).data
      var locktime: BigInteger = BigInteger.ZERO
      if (locktimeBytes == null) {
        val word = chunks.get(7).opcode
        if (word >= 82 && word <= 96)
          locktime = BigInteger.valueOf(word - 80)
        else if (locktimeBytes == null)
          return None
      } else {
        locktime = Utils.decodeMPI(Utils.reverseBytes(locktimeBytes), false)
      }
      val swapPubkey = chunks.get(4).data
      if (!chunks.get(10).isPushData)
        return None;
      val refundPubkey = chunks.get(10).data
      Some(hash, swapPubkey, refundPubkey, locktime.longValue())
    }
  }

  def get_htlc_list_inline_keyboard(htlc: HTLCFromTx,
                                    url: String,
                                    lang: String,
                                    current: Int,
                                    total: Int): InlineKeyboardMarkup = {

    println(s"Htlc keyboard for: ${htlc._3.getHashAsString}")
    if (total == 0) {
      println("Warn, empty message keyboard")
      return row_msg_keyboard(Nil, lang)
    }

    val openBtn = (Labels.OPEN_LINK, Left(url))
    val sweepBtn = (Labels.HTLC_SWEEP, Right(Callbacks.HTLC_DO_SWEEP))
    val refundBtn = (Labels.HTLC_REFUND, Right(Callbacks.HTLC_DO_REFUND))
    val viewBtn = (Labels.VIEW_CONTRACT, Right(Callbacks.VIEW_HTLC))
    val nextBtn = (Labels.NEXT, Right(Callbacks.NEXT_HTLC))
    val prevBtn = (Labels.PREV, Right(Callbacks.PREV_HTLC))

    val opts =
      if (current == 0)
        (if (total > 1) List(nextBtn) else List())
      else if (current == total - 1)
        List(prevBtn)
      else
        List(prevBtn, nextBtn)

    val row1 = mutable.ListBuffer(openBtn, viewBtn)

    // if the swap key corresponds, it's a sweep
    // if the refune key corresponds, it's a refund
    val (ps, kit, tx, utxo, redeemScript) = htlc

    val swapPubkey = redeemScript.getChunks.get(4).data
    val refundPubkey = redeemScript.getChunks.get(10).data
    val notificationKey = kit.getAccount(0).keyAt(0);

    println(s"My key: ${Utils.HEX.encode(notificationKey.getPubKey)}")
    println(s"Refund key: ${Utils.HEX.encode(utxo.getScriptPubKey.getChunks.get(10).data)}")
    println(s"SWAP key: ${Utils.HEX.encode(utxo.getScriptPubKey.getChunks.get(4).data)}")

    if (notificationKey.getPubKey.deep == swapPubkey.deep)
      if (utxo.isAvailableForSpending)
        row1 += sweepBtn

    if (notificationKey.getPubKey.deep == refundPubkey.deep)
      if (utxo.isAvailableForSpending)
        row1 += refundBtn

    val rows = List(row1.toList, opts)
    return rows_msg_keyboard(rows, lang)
  }

  def get_swaps_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options =
      List(
        Commands.LOCK_SWAP,
        Commands.HTLC_LIST,
        Commands.BACK
      ).map(command => translations((command.toString, lang)))

    make_options_keyboard(options, 3)
  }

  def listHTLCS(wallets: List[BIP47AppKit]): List[Transaction] = {
    assert(wallets.size > 0)
    val isHTLC: Transaction => Boolean = (tx: Transaction) => {
      tx.getOutputs.exists(
        out => SwapUtils.maybeRedeemableSwap(out.getScriptPubKey).isDefined)
    }
    wallets.map(_.getTransactions).flatMap(txList => txList.filter(isHTLC(_)))
  }

  def showHTLC(htlc: Transaction, wallet: BIP47AppKit, i: Int, n: Int, lang: String): String = {
    val output = htlc.getOutputs.find(out =>
      SwapUtils.maybeRedeemableSwap(out.getScriptPubKey).isDefined)

    val script = output.get.getScriptPubKey
    val isSpendable = output.get.isAvailableForSpending
    val isMine = output.get.isMine(wallet.getvWallet())

    val redeemableSwap = SwapUtils.maybeRedeemableSwap(script)
    val (hash, whoFunded, whoSwaps, locktime) = redeemableSwap.get

    val fundKey = ECKey.fromPublicOnly(whoFunded)
    val payeeKey = ECKey.fromPublicOnly(whoSwaps)
    val fundedByOwner = wallet.getvWallet().hasKey(fundKey)
    val fundedByPayee = wallet.getvWallet().hasKey(payeeKey)

    var txt = s"HTLC (${i + 1} of ${n})."
    txt += s"\n${translations(Labels.TX_HASH.toString, lang)}: ${htlc.getHashAsString}"

    htlc.getConfidence.getConfidenceType match {
      case TransactionConfidence.ConfidenceType.BUILDING => {
        txt += s"\nBlock: ${htlc.getConfidence.getAppearedAtChainHeight}"
        txt += s" (depth ${htlc.getConfidence.getDepthInBlocks})"
      }
      case c =>
        txt += s"\n**Confidence:** ${htlc.getConfidence().getDepthInBlocks}"
    }

    val scriptValue = output.get.getValue

    txt += s"\nLocktime: ${locktime}" +
      s"\nContract value: ${formatAmount(scriptValue, wallet.getCoinName)}" +
      s"\nFunded by owner: ${fundedByOwner}" +
      s"\nFunded by swap: ${fundedByPayee}" +
      s"\nIs spent: ${!isSpendable}" +
      s"\nIs mine: ${isMine}"

    txt
  }

  def viewSwaps(m: Message, lang: String)(implicit wm: WallMap): SendMessage = {
    save_state(m, States.SWAPSMENU)

    val wallets = wm.getOrElse(m.getFrom.getId, mutable.Map())
    var txt: String = "Locked:\n"
    val keyboard = Swaps.get_swaps_menu_keyboard(lang)
    println("Replying on view swaps keyboard")
    sendChooseOptionMessage(m, keyboard, lang, txt)
  }

  def messageOnSwapsMenu(m: Message, lang: String)(implicit wm: WallMap): SendMessage = {
    assert(m.hasText)
    val lockSwapCmd = getCommand(Commands.LOCK_SWAP, lang)
    val htlcListCmd = getCommand(Commands.HTLC_LIST, lang)

    m.getText match {
      case `lockSwapCmd` => {
        save_state(m, States.LOCK)
        sendChooseOptionMessage(m, Spend.get_spend_coin_menu_keyboard(m.getFrom.getId, lang), lang)
      }
      case `htlcListCmd` => {

        val wallets = wm.getOrElse(m.getFrom.getId, mutable.Map()).values.toList
        val htlcList = listHTLCS(wallets)

        // Reply with the first
        val reply = new SendMessage()
        reply.setChatId(m.getChatId)

        if (htlcList.isEmpty)
          reply.setText("Empty")
        else {
          val first = htlcList.head
          val wallet = wallets.find(_.getvWallet().getTransaction(first.getHash) != null)
          val coin = wallet.get.getCoinName

          val utxo = first.getOutputs.find(out =>
            SwapUtils.maybeRedeemableSwap(out.getScriptPubKey).isDefined
          ).get
          val redeemScript = utxo.getScriptPubKey
          val htlc = (wallet.get.getParams, wallet.get, first, utxo, redeemScript)
          val inlineKeyboard = get_htlc_list_inline_keyboard(htlc,
            getTransactionLink(coin, first), lang, 0, htlcList.size)
          reply.setReplyMarkup(inlineKeyboard)
          reply.setText(showHTLC(htlcList.head, wallet.get, 0, htlcList.size, lang))
        }

        reply
      }
    }
  }

}
