package permutas.wallets

import permutas.io.Pretty._
import permutas.constants._
import permutas.menus._
import MenuUtils._
import permutas.MongoDb._
import permutas.Types._
import permutas.i18n.translations

import reactivemongo.bson._

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import scala.collection.mutable

import org.telegram.telegrambots.meta.api.methods.send.SendMessage
import org.bitcoinj.wallet.Wallet
import org.bitcoinj.core._
import org.bitcoinj.core.bip47._
import org.bitcoinj.kits.BIP47AppKit
import org.bitcoinj.script.{Script, ScriptBuilder, ScriptChunk, ScriptPattern}
import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.crypto.TransactionSignature

package object notifications {

  def outgoingTx(coin: String, wallet: Wallet, tx: Transaction, prevBalance: Coin, newBalance: Coin, lang: String): SendMessage = {
    println("******************* BEGIN SENT NOTIFICATION ********************")
    println(s"TX: ${tx.getHashAsString}")
    val notification = new SendMessage()
    notification.enableMarkdown(true)

    var text = ""

    var msgButtonRow: List[MsgButton] = List(
      (Labels.OPEN_LINK, Left(getTransactionLink(coin, tx))),
      (Labels.TX_SET_MEMO, Right(Callbacks.SET_MEMO))
    )

    println("Outgoing tx values... ")
    println(s"Value from me: ${formatAmount(tx.getValueSentFromMe(wallet), coin)}")
    println(s"Value to me: ${formatAmount(tx.getValueSentToMe(wallet), coin)}")
    println(s"Value : ${formatAmount(tx.getValue(wallet), coin)}")

    val total = tx.getValueSentFromMe(wallet).add(tx.getFee).minus(tx.getValueSentToMe(wallet))

    println(s"Total : ${formatAmount(total, coin)}")

    // Incoming transaction
    // Transaccion saliente
    text += s"${translations((Labels.OUTGOING_TX.toString, lang))}"

    text += s"\n${translations(Labels.TX_HASH.toString, lang)}: ${formatTxLink(coin,tx)}"
    text += s"\n${translations(Labels.VALUE_SENT_FROM_ME.toString, lang)}: " +
    s"${formatAmount(tx.getValue(wallet), coin)}"

    val url = getTransactionLink(coin, tx)
    val keyboard = row_msg_keyboard(msgButtonRow, lang)

    notification.setReplyMarkup(keyboard)
    notification.setText(text)
    println("******************* END SENT NOTIFICATION ********************")
    return notification
  }

  def getP2PKHReceiveAddress(wallet: Wallet, tx: Transaction): Option[Address]
  = {
    val myOutputs = tx.getOutputs.find(_.isMineOrWatched(wallet))
    myOutputs
      .filter(_.getScriptPubKey.isSentToAddress)
      .map(_.getScriptPubKey.getToAddress(wallet.getParams, true))
  }

  def getBIP47TxChannel(address: Address,
    kit: BIP47AppKit,
    tx: Transaction): Option[BIP47Channel] = {

    val wallet = kit.getvWallet()

    // check address was derived from channel
    val bip47Address = wallet
      .getImportedKeys.map(k => LegacyAddress.fromKey(kit.getParams, k))
      .find(_.equals(address))

    if (false == bip47Address.isDefined)
      return None

    kit.getBip47MetaForAddress(address.toString) match {
      case null => None
      case chann => Some(chann)
    }
  }

  def isBIP47NotificationAddress(kit: BIP47AppKit, address: Address): Boolean = {
    val notificationAddress = kit.getAccount(0).getNotificationAddress
    address.equals(notificationAddress)
  }

  def extractOpReturnData(txOut: TransactionOutput): Option[Array[Byte]] = {
    val script = txOut.getScriptPubKey
    if (false == script.isOpReturn)
      return None
    script.getChunks
      .find(c => !c.isOpCode && c.data != null)
      .map(_.data)
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


  def incomingTx(ownerId: Int,
    coin: String,
    kit: BIP47AppKit,
    tx: Transaction,
    prevBalance: Coin,
    newBalance: Coin,
    lang: String)(implicit wm: WallMap): SendMessage = {

    println("******************* BEGIN RECEIVE NOTIFICATION ********************")

    // is change? dont be so verbose
    val valueFromMe = tx.getValueSentFromMe(kit.getvWallet())
    if (valueFromMe.isPositive) {
      println(s"Ignoring change value: ${formatAmount(valueFromMe, coin)}")

      // if it's refund, don't return
      val chunks = tx.getInput(0).getScriptSig.getChunks

      if (chunks.length <= 2) {
        println(s"Returning change with ${chunks.size()} input chunks")
        return null
      } else {
        if (chunks.size > 12 && !chunks.get(12).isPushData) {
          println(s"Returning change with ${chunks.size()} input chunks, missing 12th data")
          return null
        } else {
          // todo: extract as refundable swap
          val myPubkey = kit.getAccount(0).keyAt(0).getPubKey
          println(s"Returning? with my key: ${Utils.HEX.encode(myPubkey)}")
          println(s"Returning? with refund key: ${Utils.HEX.encode(chunks.get(10).data)}")
          println(s"Returning? with swap key: ${Utils.HEX.encode(chunks.get(4).data)}")

          val isMine = chunks.size > 10 && chunks.get(10).data.deep == myPubkey

          if (!isMine) {
            println(s"Not my refund's pubkey, returning with ${chunks.size()} input chunks")
            return null;
          }
        }
      }
    }

    val wallet = kit.getvWallet()
    val notification = new SendMessage()
    notification.enableMarkdown(true)

    println(s"My payment code: ${kit.getPaymentCode}")

    val receiveAddr = getP2PKHReceiveAddress(wallet, tx)
    val isNotificationTransaction =
      receiveAddr.fold(false)(isBIP47NotificationAddress(kit, _))
    val channel = receiveAddr.flatMap(getBIP47TxChannel(_, kit, tx))

    var buttonRow: mutable.Buffer[MsgButton] = mutable.Buffer(
      (Labels.OPEN_LINK, Left(getTransactionLink(coin, tx))),
      (Labels.TX_SET_MEMO, Right(Callbacks.SET_MEMO))
    )

    var text = ""
    if (isNotificationTransaction) {
      text += s"**${translations(Labels.INCOMING_NTX.toString, lang)}**"
      text += s"\nSender's payment code: ${kit.getPaymentCodeInNotificationTransaction(tx)}"
      buttonRow.append((Labels.ADD_AS_PAYEE, Right(Callbacks.ADD_SENDER_AS_PAYEE)))
    } else {
      text += s"**${translations(Labels.INCOMING_TX.toString, lang)}**"
    }

    println(s"Value from me: ${formatAmount(tx.getValueSentFromMe(wallet), coin)}")
    println(s"Value to me: ${formatAmount(tx.getValueSentToMe(wallet), coin)}")
    println(s"Value : ${formatAmount(tx.getValue(wallet), coin)}")
    val maybetotal = tx.getValueSentFromMe(wallet).add(tx.getFee).minus(tx.getValueSentToMe(wallet))
    println(s"Total : ${formatAmount(maybetotal, coin)}")

    text += s"\n${translations(Labels.TX_HASH.toString, lang)}: ${formatTxLink(coin, tx)}"
    val fromMe = tx.getValueSentFromMe(wallet)
    if (fromMe.isPositive())
      text += s"\n${translations(Labels.VALUE_SENT_FROM_ME.toString, lang)}: ${formatAmount(fromMe, coin)}"
    val toMe = tx.getValueSentToMe(wallet)
    if (toMe.isPositive())
      text += s"\n${translations(Labels.VALUE_SENT_TO_ME.toString, lang)}: ${formatAmount(toMe, coin)}"

    if (channel.isDefined) {

      val opret = tx.getOutputs.find(_.getScriptPubKey.isOpReturn)

      val limitSwap: Option[LimitSwap]
      = opret.flatMap(extractOpReturnData).flatMap(Swaps.SwapUtils.maybeLimitSwap)

      if (limitSwap.isDefined) {
        val (wantCoin, wantPrice, wantLocktime, wantSecretHash) = limitSwap.get
        val inlineKeyboard = Swaps.get_limit_swap_inline_keyboard(lang)
        notification.setReplyMarkup(inlineKeyboard)
        println(s"Detected limit swap: ${limitSwap.get}")

        text += s"\nClient ID: ${channel.get.getPaymentCode}"
        text += s"\nWants to buy coin: ${wantCoin}"
        text += s"\nWith price: ${wantPrice}"
        text += s"\nTime to live: ${wantLocktime}"
        text += s"\nHash locked by: ${wantSecretHash}"

        watchLimitSwap(kit, channel.get, limitSwap.get._4)
        save_db(Collections.USER_HTLC, ownerId, wantSecretHash.toString, BSONDocument(
          "limit_tx" -> tx.getHashAsString,
          "want_coin" -> BSONString(wantCoin),
          "want_price" -> BSONDouble(wantPrice),
          "want_locktime" -> BSONLong(wantLocktime),
          "swap_payee" -> BSONString(channel.get.getPaymentCode),
          "swap_payer" -> BSONString(kit.getPaymentCode)
        ))

        buttonRow += ((Labels.HTLC_FILL, Right(Callbacks.SWAP_COUNTER_LOCK)))
      } else {
        println("Could not detect limit swap")
      }
    }

    // Received a swap that is reedemable?
    val swapRedeemable: Option[RedeemableSwap] = tx.getOutputs.flatMap(out =>
      Swaps.SwapUtils.maybeRedeemableSwap(out.getScriptPubKey)).headOption
    if (swapRedeemable.isDefined) {
      val (hash, whoFunded, whoSwaps, locktime) = swapRedeemable.get

      // owner can sweep when it has the secret, otherwise offer to counter lock
      val secret_value = get_collection_string_value(
        Collections.USER_HTLC, ownerId, "swap_secret", data_key = s"${hash.toString}")

      if (false == secret_value.isDefined)
        buttonRow += ((Labels.HTLC_FILL, Right(Callbacks.SWAP_COUNTER_LOCK)))
      else
        buttonRow += ((Labels.HTLC_SWEEP, Right(Callbacks.HTLC_DO_SWEEP)))
    }

    // tx is incoming, because payer did sweep?
    if (receiveAddr.isDefined) {
      println(s"Checking for sweep, owner: ${ownerId}")

      val txInputs = tx.getInputs.filter(
        input => input.getOutpoint != null
          && input.getConnectedOutput != null
      )
      for (input <- txInputs) {
        val outpoint = input.getOutpoint
        val connectedOutput = outpoint.getConnectedOutput
        val txBeingSpent = connectedOutput.getParentTransaction

        //println(s"TX BEING SPENT: ${txBeingSpent.getHashAsString}")

        val script = connectedOutput.getScriptPubKey
        val outChunks = script.getChunks

        // The input script (3 chunks) has the 20 bytes secret
        // in position 1
        var secretBytes = Array[Byte]()

        val inChunks = input.getScriptSig.getChunks
        if (inChunks.length == 3 && inChunks.get(1).isPushData) {
          secretBytes = inChunks.get(1).data
          if (secretBytes.size == 20) {
            println("Found secret")
          }
        }

        // find the hash based on the tx being spent.
        val maybeSecretHash: Option[Sha256Hash] =
          if (secretBytes.nonEmpty)
            Some(Sha256Hash.of(secretBytes))
          else
            None

        if (maybeSecretHash.isDefined) {
          val secretHash = maybeSecretHash.get
          val secretHashCounterTx = get_collection_string_value(
            collection = Collections.USER_HTLC,
            who = ownerId,
            key = "countertx",
            data_key = secretHash.toString
          ).get

          val secretHashBaseTx = get_collection_string_value(
            collection = Collections.USER_HTLC,
            who = ownerId,
            key = "basetx",
            data_key = secretHash.toString
          ).get

          val limitSwap = get_collection_doc_value(
            Collections.USER_HTLC, ownerId, data_key = secretHash.toString
          )

          val swap_coin = limitSwap.get.get("want_coin").get.asInstanceOf[BSONString]
          val want_network = networks(swap_coin.value)
          val kits = wm.getOrElse(ownerId, mutable.Map()).values.toList

          val wallet: BIP47AppKit = kits.find(
            wallet => wallet
              .getParams.equals(want_network)).get

          val baseHtlcTx = wallet.getTransactions.find(_.getHashAsString.equals(secretHashBaseTx))
          val counterHtlcTx = wallet.getTransactions.find(_.getHashAsString.equals(secretHashCounterTx))

          if (baseHtlcTx.isDefined) {

            val maybeUtxo = baseHtlcTx.get.getOutputs.find(
              out => Swaps.SwapUtils
                .maybeRedeemableSwap(out.getScriptPubKey).isDefined)
            val utxo = maybeUtxo.get

            val redeemableSwap = Swaps.SwapUtils.maybeRedeemableSwap(utxo.getScriptPubKey)

            val swap_payee = get_collection_string_value(
              Collections.USER_HTLC, ownerId, "swap_payee", secretHash.toString)

            val secret = secretBytes
            val swap = new Swap(kit.getParams) //TODO: get the right params
            val sweepScript = swap.getSwapInputScript(script, secret)

            val ps = kit.getParams

            val outpoint = new TransactionOutPoint(ps, utxo.getIndex, utxo.getParentTransactionHash)
            val txInput = new TransactionInput(ps, baseHtlcTx.get, sweepScript.getProgram, outpoint, utxo.getValue)

            val spendTx = new Transaction(ps)
            spendTx.addInput(txInput)

            val feePerKb = if (!wallet.getParams.getUseForkId)
              Transaction.DEFAULT_TX_FEE
            else
              Transaction.BCC_DEFAULT_TX_FEE

            spendTx.addOutput(utxo.getValue, wallet.getCurrentAddress)
            val feeAmount = feePerKb.div(1000).multiply(spendTx.getMessageSize);
            spendTx.getOutput(0).setValue(spendTx.getOutput(0).getValue.minus(feeAmount))

            spendTx.verify()
            spendTx.setVersion(2)

            val sighash = spendTx.hashForSignature(0, utxo.getScriptPubKey.getProgram, Transaction.SigHash.ALL.byteValue())
            val signKey = wallet.getAccount(0).keyAt(0)
            val mySig: ECKey.ECDSASignature = signKey.sign(sighash)
            val txSig = new TransactionSignature(mySig, SigHash.ALL, false)

            val reinputScript = new ScriptBuilder()
              .data(txSig.encodeToBitcoin())
              .data(secret)
              .smallNum(1).build()

            try {
              reinputScript.correctlySpends(spendTx, 0,
                utxo.getScriptPubKey, Script.ALL_VERIFY_FLAGS)
            } catch {
              case e: Throwable => { println(s"Fails to spend: ${e.printStackTrace()}"); throw e }
            }

            spendTx.clearInputs()
            val txInput2 = new TransactionInput(ps, baseHtlcTx.get, reinputScript.getProgram, outpoint, utxo.getValue)
            txInput.setScriptSig(reinputScript)
            spendTx.addInput(txInput2)
            assert(spendTx.getInputs.size == 1)

            assert(spendTx.getFee.isPositive)
            println(Utils.HEX.encode(spendTx.bitcoinSerialize()))
            wallet.broadcastTransaction(spendTx)
          }

          if (counterHtlcTx.isDefined) {
            println("Bingo counter")
          }

          val refundKey = script.getChunks.get(10).data
          val notificationPubkey = kit.getAccount(0).keyAt(0).getPubKey
          if (refundKey.deep == notificationPubkey.deep) {
            println("Bingo refund key")
          }
        }
      }
    }

    notification.setReplyMarkup(row_msg_keyboard(buttonRow, lang))
    println("******************* END RECEIVE NOTIFICATION ********************")
    notification.setText(text)
    return notification
  }
}
