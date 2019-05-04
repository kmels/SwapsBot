import java.io.{File, Serializable}
import java.math.BigInteger
import java.net.InetAddress
import java.security.SecureRandom
import java.util.Date

import Main.{CallbackName, _}
import com.google.common.net.InetAddresses
import io.nayuki.qrcodegen.QrCode
import org.bitcoinj.core._
import org.bitcoinj.core.bip47.{BIP47Account, BIP47Channel, BIP47PaymentCode}
import org.bitcoinj.kits.BIP47AppKit
import org.bitcoinj.params._
import org.bitcoinj.script.{Script, ScriptBuilder, ScriptChunk, ScriptPattern}
import org.telegram.telegrambots.ApiContextInitializer
import org.telegram.telegrambots.meta.api.methods.BotApiMethod
import org.telegram.telegrambots.bots.{DefaultBotOptions, TelegramLongPollingBot}
import org.telegram.telegrambots.meta.api.methods.send.{SendMessage, SendPhoto}
import org.telegram.telegrambots.meta.TelegramBotsApi
import org.telegram.telegrambots.meta.api.objects.{Message, Update}
import org.telegram.telegrambots.meta.generics.BotOptions

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.api.{Cursor, DefaultDB, MongoConnection, MongoDriver}
import reactivemongo.bson.{BSONDocumentReader, BSONDocumentWriter, Macros, document}
import reactivemongo.bson.BSONDocument
import org.telegram.telegrambots.meta.api.objects.replykeyboard.{InlineKeyboardMarkup, ReplyKeyboardMarkup}
import org.telegram.telegrambots.meta.exceptions.TelegramApiException

import scala.concurrent.Future
import reactivemongo.bson._
import reactivemongo.api.collections.bson.BSONCollection

import mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent._
import org.bitcoinj.wallet.{DeterministicSeed, SendRequest, Wallet}
import org.bitcoinj.wallet.listeners.{ScriptsChangeEventListener, WalletCoinsReceivedEventListener, WalletCoinsSentEventListener}
import org.bitcoinj.script.ScriptOpCodes._
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.InlineKeyboardButton
import org.telegram.telegrambots.meta.api.methods.send.SendMessage
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.InlineKeyboardButton
import java.util

import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.crypto.TransactionSignature
import org.bitcoinj.utils.BIP47Util
import org.telegram.telegrambots.meta.api.methods.updatingmessages.EditMessageText

/*

// Sun 4/14/2019
//    todo: Create order book commands for channels
//        /bid 20 BCH 1 BTC
//        /bid BTCBCH 0.05 0.50 BCH
//        /bid BTCBCH 0.06 0.20 BCH
//           =>  ------ BTC/BCH -------
//           best ask:  |      |          |
//           last swap: |
//           best bid:  | 0.06 | 0.50 BCH |
//           best bid:  | 0.05 | 0.20 BCH |
//
//        /ask 20 BCH 2 BTC
//        /ask BTCBCH 0.12 2
//        /ask BTCBCH 0.10 1

//           =>  ------ BTC/BCH -------
//           asks:      | price | available |
//           best ask:  | 0.12  | 16.66 BCH |
//           best ask:  | 0.10  | 10.00 BCH |
//           last swap: |
//           best bid:  | 0.06  | 0.50 BCH  |
//           best bid:  | 0.05  | 0.20 BCH  |

//        /fill ask 0.10 5 BCH
//          // cancels ask, ask becomes 0.10 | 5 BCH

// backlog:
//    todo: buy domain xcswaps.com (.5h)
//    todo: show srchut wiki (3h)
//       validate scripts with 'unlockable' address, must be p2sh!
//    todo: add via pubkey ?
//    todo: (maybe add docs for txs prev and next pagination) (2h, 0.5h)
////
//    feature: send in location (can search offers by location)
 */

// viernes 20
// TODO: Restaurar desde semilla
// TODO: Navegacion en lista de direcciones

object SwapUtils {
  val REFUND_TIME_NEEDED = 2; //24*60 //2;

  def maybeLimitSwap(data: Array[Byte]): Option[LimitSwap] = {
    if (data.size != 48)
      return None

    val coinBytes = data.take(4)
    val coinStartByte = if (coinBytes(0)=='-'.toByte) 1 else 0
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
    val hash : Sha256Hash = new Sha256Hash(chunks.get(2).data)
    if (!chunks.get(4).isPushData)
      return None
    if (!chunks.get(7).isPushData) {
      return None
    }
    var locktimeBytes = chunks.get(7).data
    var locktime : BigInteger = BigInteger.ZERO
    if (locktimeBytes == null) {
      val word = chunks.get(7).opcode
      if (word >= 82 && word <= 96)
        locktime = BigInteger.valueOf(word - 80)
      else
        if (locktimeBytes == null)
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
class Swap(params: NetworkParameters) {
  type Secret = Array[Byte]
  var secret: Option[Secret] = None

  def getRedeemScript(hash: Sha256Hash,
                      refundPubKey: Array[Byte],
                      swapPubKey: Array[Byte], locktime: Long): Script = {

    println("Get redeem script ...")
    println(s"Hash = ${Utils.HEX.encode(hash.getBytes)}")
    println(s"Refund Pubkey = ${Utils.HEX.encode(refundPubKey)}")
    println(s"Swap Pubkey = ${Utils.HEX.encode(swapPubKey)}")
    println(s"Locktime = ${locktime}")

    new ScriptBuilder()
      .op(OP_IF)
      .op(OP_SHA256)
      .data(hash.getBytes)
      .op(OP_EQUALVERIFY)
      .data(swapPubKey)
      .op(OP_CHECKSIG)
      .op(OP_ELSE)
      .number(locktime)
      .op(OP_CHECKSEQUENCEVERIFY)
      .op(OP_DROP)
      .data(refundPubKey)
      .op(OP_CHECKSIG)
      .op(OP_ENDIF)
      .build()
  }

  def getAddressFromRedeemScript(redeemScript: Script) : Address= {
    ScriptBuilder.createP2SHOutputScript(redeemScript).getToAddress(params);
  }


  // 48 bytes:
  // [want coin][price][locktime][secret]
  // [4bytes][4bytes][8 byte int][32bytes] = [48 bytes]
  // want coin ::  String
  // price :: want/have coin, e.g. want 20 tBCH for 1 tBTC (price = 20)
  def getLimitSwapPayload(wantCoin: String, price: Float,
                          locktime: Long, secretHash:Array[Byte]): Array[Byte] = {
    val ret = mutable.ArrayBuffer[Byte]()

    assert(wantCoin.size == 3 || wantCoin.size == 4)

    val wantBytes = wantCoin.getBytes
    if (wantCoin.size==3) {
      ret.update(0, '-'.toByte)
    }
    ret ++= wantBytes
    assert(ret.size == 4)

    val floatBytes = java.nio.ByteBuffer.allocate(4)
    floatBytes.putFloat(price)
    ret ++= floatBytes.array()
    assert(ret.size == 8)

    val locktimeBytes = java.nio.ByteBuffer.allocate(8)
    locktimeBytes.putLong(locktime)
    ret ++= locktimeBytes.array()
    assert(ret.size == 16)

    assert(secretHash.size == 32)
    ret ++= secretHash
    assert(ret.size == 48)
    ret.toArray
  }

  def getRefundInputScript(redeemScript: Script): Script = {
    new ScriptBuilder()
      .smallNum(0)
      .smallNum(0)
      .build()
  }

  def getSwapInputScript(redeemScript: Script, secret: Array[Byte]): Script = {
    new ScriptBuilder()
      .smallNum(0)
      .data(secret)
      .smallNum(1)
      .build()
  }

  def extractSecret(tx: Transaction, address: Address): Option[Array[Byte]] = {
    for (input <- tx.getInputs) {
      if (input.getOutpoint.getConnectedOutput.getAddressFromP2SH(params).equals(address))
        return Some(input.getScriptSig.getChunks.get(1).data)
    }
    return None
  }

  def extractOutput(tx: Transaction, address: Address): Option[TransactionOutput] = {
    for (out <- tx.getOutputs) {
      val outputAddress = out.getAddressFromP2SH(params)
      if (outputAddress.equals(address))
        return Some(out)
    }
    return None
  }

  def makeSecret(): Array[Byte] = {
    val secret_ = new ECKey().getPrivKeyBytes.take(20)
    assert(secret_.size==20)
    this.secret = Some(secret_)
    return secret_
  }

  def getSecret(): (Array[Byte],Sha256Hash) = {
    if (false == this.secret.isDefined)
      makeSecret()

    (this.secret.get, Sha256Hash.of(this.secret.get))
  }
}

object Main {
  /*
   * Wallets
   *   1 BSV
   *   0.15 BCH
   *   0.0157 BTC
   *
   *   To pay (Locked):
   *      0.0157 BTC (05/15)
   *   To collect (Locked):
   *      1 BSV (05/15)
   *
   *   * Transactions
   *   * Mnemonics
   *   * Addresses
   *   * BCH
   *   * BSV
   *
   * Spend
   *   Choose an option
   *     * BTC
   *     * BCH
   *     * BSV
   *     Send amount: 0.0001 (min: 0.0001 BSV)
   *       * -5%
   *       * -1%
   *       * +1%
   *       * +5%
   *     Enter address:
   *
   * Lock in someone's inbox
   *   Choose
   *   * BTC
   *   * BCH
   *   * BSV
   *     Choose option:
   *       * 6H Swap
   *          * Enter address:
   *       * 3D Loan
   *          * Enter collateral ratio (min: 1.0)
   *
   *       * 7D Forward
   *       * Custom
   *       Enter amount:
   * Lock request
   *
   * Contacts
   *
   * Settings
   *   * Payment code
   *   * Mnemonic code
   *   * Language
   *   * Manage accounts ?
   *       * Create Account
   *           * BSV
   *           * BTC
   *       * List accounts
   */
  var api_token = ""
  var run_env = "demo"
  val adminId = 111720349
  val faucetCoin = Coin.valueOf(0, 2)

  object CommandName extends Enumeration {
    val ADD_PAYEE, ADDRESSLIST, BACK, BROADCAST, BORROW, CANCEL, ENCODE, ENGLISH_LANG, FAUCET,
    GET_PAYEE, HTLCLIST, INFO, LANGUAGE, LOAN, LOCK, LOCK_SWAP, LOCK_LOAN, PAYEES, PEERS, RESTORE, RATE, RESCAN,
    SEED, SETTINGS, SPANISH_LANG, SPEND, SWAPS, SWEEP, TRANSACTIONS, VIEWSCRIPT, WALLETS, WHOAMI = Value
  }

  object CallbackName extends Enumeration {
    val ADD_SENDER_AS_PAYEE,
    HTLC_DATE_SORT, HTLC_AMOUNT_SORT, HTLC_DO_SWEEP, HTLC_DO_REFUND, NEXT_HTLC, OPEN_TX_LINK,
    PREV_HTLC, SET_MEMO, SWAP_LIMIT_FILL, SWAP_COUNTER_LOCK, VIEW_HTLC = Value
  }

  object StateName extends Enumeration {
    val ADD_NEW_PAYEE, ADD_NEW_PAYEE_ADDRESS, ADDRESS_LIST, BROADCAST_PAYEE_NTX,
    FAUCETMENU, LANGUAGE, LANGUAGEMENU, LOCK, LOCK_COIN, LOCK_COIN_SCRIPT, LOCK_COIN_SCRIPT_TO,
    LOCK_COIN_SCRIPT_TO_AMOUNT, LOCK_CONFRMATION, MAINMENU,
    SETTINGSMENU, SPEND, SPEND_COIN, SPEND_COIN_TO, SPEND_COIN_TO_AMOUNT,
    SPEND_COIN_TO_NTX, SWAPSMENU, STARTSTATE, UNITS, WALLET_PEERS, WALLETMENU, WHOAMIMENU = Value
  }

  object Texts extends Enumeration {
    val ADD_AS_PAYEE, BROADCAST_CONTRACT_CONFIRM, BROADCAST_TX_CONFIRM, CREATE_CHANNEL_CONFIRM,
    ENTER_SPEND_AMOUNT, HTLC_OFFER, HTLC_REFUND, HTLC_SWEEP, INCOMING_TX, INCOMING_NTX, NEXT, OPEN_LINK, OUTGOING_TX,
    PREV, TX_HASH, TX_SET_MEMO, SWAP_FILL, VALUE_SENT_TO_ME,
    VALUE_SENT_FROM_ME, VIEW_CONTRACT = Value
  }

  object CollectionName extends Enumeration {
    val LANGUAGE_COLLECTION = Value("Language")
    val USER_HTLC = Value("HTLC")
    val USER_META_COLLECTION = Value("Meta")
    val USER_STATE_COLLECTION = Value("State")
    val USER_PAYEE_COLLECTION = Value("Payee")
  }

  case class CatchError(val error: Errata.Value) extends Exception
  case class SendPhotoCatch(val sendPhoto: SendPhoto) extends Exception


  val translations : Map[(String, String), String] = Map(
    // Commands
    (CommandName.ADD_PAYEE.toString, "en") -> "➕ Add",
    (CommandName.ADD_PAYEE.toString, "es") -> "➕ Agregar",
    (CommandName.ADDRESSLIST.toString, "en") -> "\uD83D\uDCD6 Addressses",
    (CommandName.ADDRESSLIST.toString, "es") -> "\uD83D\uDCD6 Direcciones",
    //(CommandName.ACTIVE_SWAPS.toString, "en") -> "\uD83D\uDD02 Current swaps",
    //(CommandName.ACTIVE_SWAPS.toString, "es") -> "\uD83D\uDD02 Permutas vigentes",
    (CommandName.BACK.toString, "en") -> "\uD83D\uDD19 Back",
    (CommandName.BACK.toString, "es") -> "\uD83D\uDD19 Atrás",
    (CommandName.BROADCAST.toString, "en") -> "\uD83D\uDC4C Send" ,
    (CommandName.BROADCAST.toString, "es") -> "\uD83D\uDC4C Enviar" ,
    (CommandName.BORROW.toString, "en") -> "\uD83D\uDD16 Borrow",
    (CommandName.BORROW.toString, "es") -> "\uD83D\uDD16 Pedir prestado",
    (CommandName.CANCEL.toString, "en") -> "❌ Cancel" ,
    (CommandName.CANCEL.toString, "es") -> "❌ Cancelar",
    (CommandName.ENCODE.toString, "en") -> "\uD83D\uDCDD Encode",
    (CommandName.ENCODE.toString, "es") -> "\uD83D\uDCDD Codificar",
    (CommandName.ENGLISH_LANG.toString, "en") -> "\uD83C\uDDEC\uD83C\uDDE7 English",
    (CommandName.ENGLISH_LANG.toString, "es") -> "\uD83C\uDDEC\uD83C\uDDE7 Inglés",
    (CommandName.FAUCET.toString, "en") -> "\uD83D\uDEB0 Faucet",
    (CommandName.FAUCET.toString, "es") -> "\uD83D\uDEB0 Grifo",
    (CommandName.GET_PAYEE.toString, "en") -> "\uD83D\uDCD6 Addresses",
    (CommandName.GET_PAYEE.toString, "es") -> "\uD83D\uDCD6 Direcciones",
    (CommandName.HTLCLIST.toString, "en") -> "⏳ Contracts",
    (CommandName.HTLCLIST.toString, "es") -> "⏳ Contratos",
    (CommandName.INFO.toString, "en") -> "ℹ Info",
    (CommandName.INFO.toString, "es") -> "ℹ Información",
    (CommandName.LANGUAGE.toString, "en") -> "\uD83C\uDF10 Language",
    (CommandName.LANGUAGE.toString, "es") -> "\uD83C\uDF10 Idioma",
    (CommandName.LOAN.toString, "en") -> "\uD83D\uDCD6 Loan",
    (CommandName.LOAN.toString, "es") -> "\uD83D\uDCD6 Prestar",
    (CommandName.LOCK.toString, "en") -> "\uD83D\uDDDD Lock",
    (CommandName.LOCK.toString, "es") -> "\uD83D\uDDDD Inmobilizar",
    (CommandName.LOCK_SWAP.toString, "en") -> "➕ Swap",
    (CommandName.LOCK_SWAP.toString, "es") -> "➕ Permutar",
    (CommandName.PAYEES.toString, "en") -> "\uD83D\uDC64 Payees",
    (CommandName.PAYEES.toString, "es") -> "\uD83D\uDC64 Beneficiarios",
    (CommandName.PEERS.toString, "en") -> "\uD83D\uDC65 Peers",
    (CommandName.PEERS.toString, "es") -> "\uD83D\uDC65 Pares",
    (CommandName.RATE.toString, "en") -> "Rate",
    (CommandName.RATE.toString, "es") -> "Calificar",
    (CommandName.RESTORE.toString, "en") -> "\uD83D\uDCE6 Restore",
    (CommandName.RESTORE.toString, "es") -> "\uD83D\uDCE6 Restaurar",
    (CommandName.RESCAN.toString, "en") -> "♻ Rescan",
    (CommandName.RESCAN.toString, "es") -> "♻ Sintonizar",
    (CommandName.SEED.toString, "en") -> "\uD83E\uDD51 Seed",
    (CommandName.SEED.toString, "es") -> "\uD83E\uDD51 Semilla",
    //(CommandName.SWAPS.toString, "en") -> "\uD83D\uDD02 Swaps",
    //(CommandName.SWAPS.toString, "es") -> "\uD83D\uDD02 Permutas",
    (CommandName.SETTINGS.toString, "en") -> "\uD83D\uDD27 Settings",
    (CommandName.SETTINGS.toString, "es") -> "\uD83D\uDD27 Ajustes",
    (CommandName.SPANISH_LANG.toString, "en") -> "\uD83C\uDDEA\uD83C\uDDF8 Spanish",
    (CommandName.SPANISH_LANG.toString, "es") -> "\uD83C\uDDEA\uD83C\uDDF8 Español",
    (CommandName.SPEND.toString, "en") -> "▶ Spend",
    (CommandName.SPEND.toString, "es") -> "▶ Gastar",
    (CommandName.TRANSACTIONS.toString, "en") -> "\uD83D\uDD87 Transactions",
    (CommandName.TRANSACTIONS.toString, "es") -> "\uD83D\uDD87 Transacciones",
    (CommandName.VIEWSCRIPT.toString, "en") -> "\uD83D\uDCDD Script",
    (CommandName.VIEWSCRIPT.toString, "es") -> "\uD83D\uDCDD Escrito",
    (CommandName.WALLETS.toString, "en") -> "\uD83D\uDC5C Wallets",
    (CommandName.WALLETS.toString, "es") -> "\uD83D\uDC5C Billeteras",
    (CommandName.WHOAMI.toString, "en") -> "\uD83D\uDC7B Who am I", //👻
    (CommandName.WHOAMI.toString, "es") -> "\uD83D\uDC7B Quién soy", //💻

    // Errata
    (Errata.NEEDS_START.toString, "en") -> "Click /start",
    (Errata.NEEDS_START.toString, "es") -> "Haz /start",
    (Errata.INVALID_COIN.toString, "en") -> "Pick one coin from the menu.",
    (Errata.INVALID_COIN.toString, "es") -> "Escoge una moneda del menu.",
    (Errata.INVALID_AMOUNT.toString, "en") -> "Send an amount like `0.1022 tBCH` or a percentage like `2.5%`",
    (Errata.INVALID_AMOUNT.toString, "es") -> "Envia una cantidad como `0.22` o un porcentaje como `2.5%`",
    (Errata.INVALID_RECIPIENT.toString, "en") -> "Enter a valid address or choose a contact ",
    (Errata.INVALID_RECIPIENT.toString, "es") -> "Ingresa una direccion o escoge un contacto",
    (Errata.MISSING_SPEND_COINS.toString, "en") -> s"To receive, click 🚰 Faucet",
    (Errata.MISSING_SPEND_COINS.toString, "es") -> s"Para recibir, clic en 🚰 Grifo",

    (Texts.ADD_AS_PAYEE.toString, "en") -> "➕ Add",
    (Texts.ADD_AS_PAYEE.toString, "es") -> "➕ Agregar",
    (Texts.CREATE_CHANNEL_CONFIRM.toString, "en") -> "Do you want to stablish a channel?",
    (Texts.CREATE_CHANNEL_CONFIRM.toString, "es") -> "¿Querés establecer un canal?",
    (Texts.BROADCAST_CONTRACT_CONFIRM.toString, "en") -> "Do you want to broadcast the contract?",
    (Texts.BROADCAST_CONTRACT_CONFIRM.toString, "es") -> "¿Querés transmitir el contrato?",
    (Texts.BROADCAST_TX_CONFIRM.toString, "en") -> "Do you want to broadcast the transaction?",
    (Texts.BROADCAST_TX_CONFIRM.toString, "es") -> "¿Querés transmitir la transacción?",
    (Texts.ENTER_SPEND_AMOUNT.toString, "en") -> "Enter spend amount:",
    (Texts.ENTER_SPEND_AMOUNT.toString, "es") -> "Ingresa la cantidad a gastar:",
    (Texts.HTLC_OFFER.toString, "en") -> "Offer",
    (Texts.HTLC_OFFER.toString, "es") -> "Ofertar",
    (Texts.HTLC_REFUND.toString, "en") -> "Refund",
    (Texts.HTLC_REFUND.toString, "es") -> "Rembolsar",
    (Texts.HTLC_SWEEP.toString, "en") -> "Sweep",
    (Texts.HTLC_SWEEP.toString, "es") -> "Barrer",
    (Texts.INCOMING_NTX.toString, "en") -> "🔔 Notification transaction",
    (Texts.INCOMING_NTX.toString, "es") -> "🔔 Transacción de notificación",
    (Texts.INCOMING_TX.toString, "en") -> "🔔 Incoming transaction",
    (Texts.INCOMING_TX.toString, "es") -> "🔔 Transacción entrante",
    (Texts.NEXT.toString, "en") -> "Next",
    (Texts.NEXT.toString, "es") -> "Siguiente",
    (Texts.OPEN_LINK.toString, "en") -> "Open", //Block explorer
    (Texts.OPEN_LINK.toString, "es") -> "Abrir", //Explorar
    (Texts.OUTGOING_TX.toString, "en") -> "🔔 Outgoing transaction",
    (Texts.OUTGOING_TX.toString, "es") -> "🔔 Transacción saliente",
    (Texts.PREV.toString, "en") -> "Prev",
    (Texts.PREV.toString, "es") -> "Anterior",
    (Texts.SWAP_FILL.toString, "en") -> "Fill Swap",
    (Texts.SWAP_FILL.toString, "es") -> "Concretar Permuta",
    (Texts.TX_HASH.toString, "en") -> "Hash",
    (Texts.TX_HASH.toString, "es") -> "Hash",
    (Texts.TX_SET_MEMO.toString, "en") -> "Set Memo",
    (Texts.TX_SET_MEMO.toString, "es") -> "Poner Nota",
    (Texts.VALUE_SENT_FROM_ME.toString, "en") -> "Value",
    (Texts.VALUE_SENT_FROM_ME.toString, "es") -> "Valor",
    (Texts.VALUE_SENT_TO_ME.toString, "en") -> "Value",
    (Texts.VALUE_SENT_TO_ME.toString, "es") -> "Valor",
    (Texts.VIEW_CONTRACT.toString, "en") -> "View",
    (Texts.VIEW_CONTRACT.toString, "es") -> "Ver"
  )

  object Errata extends Enumeration {
    // are catched
    val NEEDS_START = Value
    val NEEDS_BIP47_CHANNEL = Value

    // are rendered
    val INVALID_COIN, INVALID_AMOUNT, INVALID_LANGUAGE,
    INVALID_RECIPIENT, INSUFFICIENT_BALANCE = Value

    val EMPTYFAUCET = Value("Faucet error: Not enough coins.")
    val FAUCET_NEEDS_ADMIN = Value("Faucet error: Admin does not have wallets")
    val MISSING_SPEND_COINS = Value

    // state machine errors
    val MISSING_META_COIN = Value("State machine error: Missing meta's coin. Please go to /start")
    val MISSING_META_PAYEE = Value("State machine error: Missing meta's payee. Please go to /start")
    val MISSING_META_AMOUNT = Value
  }

  import CollectionName._
  import StateName._

  //type Doc = org.mongodb.scala.bson.collection.Document
  type Who = Integer
  type Pubkey = Array[Byte]
  type UserWallets = mutable.Map[String, BIP47AppKit]
  type LimitSwap = (String, Float, Long, Sha256Hash)
  type HTLCFromTx = (NetworkParameters, BIP47AppKit,Transaction,TransactionOutput,Script)
  type KeyboardI18NButton = (Texts.Value, Either[String,CallbackName.Value])

  // hash, who swaps, who reunds, locktime
  type RedeemableSwap = (Sha256Hash, Pubkey, Pubkey, Long)
  val wallMap : mutable.Map[Int, mutable.Map[String, BIP47AppKit]] = mutable.Map()
  val networks : Map[String, NetworkParameters] = Map(
    "BSV" -> new BSVMainNetParams(),
    "BCH" -> new BCCMainNetParams(),
    "tBCH" -> new BCCTestNet3Params(),
    "BTC" -> new MainNetParams(),
    "tBTC" -> new TestNet3Params()
  )


  /******
  * BEGIN DATABASE LAYER
  *******/

  val mongoUri = "mongodb://localhost:27018"///xcswaps_dev"
  import ExecutionContext.Implicits.global

  val driver = MongoDriver()
  val parsedUri = MongoConnection.parseURI(mongoUri)
  val connection = parsedUri.map(driver.connection(_))

  val futureConnection = Future.fromTry(connection)
  def db = futureConnection.flatMap(_.database(s"xcswaps_${run_env}"))
  def col(s: CollectionName.Value)  = db.map(_.collection(s.toString()))

  def get_collection_value(collection: CollectionName.Value, who: Who) : Option[BSONValue] = {
    get_collection_value(collection, who, None, None)
  }

  def get_collection_value(collection: CollectionName.Value, who: Who, key: String) : Option[BSONValue] = {
    get_collection_value(collection, who, Some(key), None)
  }

  def get_collection_doc_value(collection: CollectionName.Value, who: Who, data_key: String) : Option[BSONDocument] = {
    get_collection_value(collection, who, data_key = data_key) match {
      case Some(BSONDocument(e)) => {
        Some(BSONDocument(e))
      }
      case _ => None
    }
  }

  def get_collection_string_value(collection: CollectionName.Value,
                                  who: Who, key: String, data_key: String = "data") : Option[String] = {
    get_collection_value(collection, who, Some(key), None, data_key = data_key) match {
      case Some(BSONString(s)) => Some(s)
      case _ => None
    }
  }

  def get_collection_value(collection: CollectionName.Value,
                          who: Who,
                          key: String,
                          fallback_value: BSONValue): Option[BSONValue] = {
    get_collection_value(
      collection,
      who,
      if (key.isEmpty) None else Some(key),
      Some(fallback_value))
  }

  def get_collection_value(collection: CollectionName.Value,
                           who: Who, doc_key: Option[String] = None,
                           fallback_value: Option[BSONValue] = None,
                           data_key : String = "data"
                          ): Option[BSONValue] = {

    // Accept key "data.key" instead of just "key" and a hardcoded data
    var ret : BSONDocument = null
    val query = BSONDocument("who"->BSONInteger(who))
    val doc : Future[Option[BSONDocument]] = col(collection).flatMap(_.find(query).one[BSONDocument])

    val it: Option[BSONDocument] = Await.result(doc, 2 seconds)

    val transformed: Option[BSONValue] = it.flatMap(bson => {

      val (bsondata, key) : (BSONValue, Option[String]) =
      if (false == bson.contains(data_key)) {
        val doc = bson.get(data_key.takeWhile(_!='.')).get
        val dockey = Some(data_key.dropWhile(_!='.').tail)
        (doc, dockey)
      }
      else {
        (bson.get(data_key).get, doc_key)
      }

      bsondata match {
        case data: BSONDocument =>
          if (key.isDefined && data.contains(key.get)) {
            val ret = Some(data.get(key.get).get)
            ret
          } else {
            Some(data)
          }
        case arr: BSONArray => Some(arr)
        case t => {
          println(s"WARN: Unexpected get_collection_value: ${t} ")
          None
        }
      }
    })

    transformed.fold(fallback_value)(x => Some(x))
  }

  /******
  * END DATABASE LAYER
  *******/

  def getHelpMessage(lang: String) : String = ""
  def getWalletsCommand(lang: String) : String = lang match {
    case "es" => "\uD83D\uDC5C Billeteras"
    case "en" => "\uD83D\uDC5C Wallets"
    case _ => "\uD83D\uDC5C Wallets"
  }

  def getAddPayeeCommand(lang: String) = translations(CommandName.ADD_PAYEE.toString,lang)
  def getAddressListCommand(lang: String) = translations((CommandName.ADDRESSLIST.toString,lang))
  def getBackCommand(lang: String) : String = "\uD83D\uDD19 Back" //❌\
  def getBroadcastCommand(lang: String) = translations((CommandName.BROADCAST.toString,lang))
  def getCancelCommand(lang: String) = translations((CommandName.CANCEL.toString,lang))
  def getFaucetCommand(lang: String) = translations((CommandName.FAUCET.toString,lang))
  def getEncodeCommand(lang: String) = translations((CommandName.ENCODE.toString,lang))
  def getEnglishLanguageCommand(lang: String) = translations((CommandName.ENGLISH_LANG.toString,lang))
  def getHTLCListCommand(lang: String) = translations((CommandName.HTLCLIST.toString,lang))
  def getInfoCommand(lang: String) = translations((CommandName.INFO.toString,lang))
  def getLanguageCommand(lang: String) = translations((CommandName.LANGUAGE.toString,lang))
  def getLockCommand(lang: String) = translations((CommandName.LOCK.toString,lang))
  def getLockSwapCommand(lang: String) = translations((CommandName.LOCK_SWAP.toString,lang))
  def getLockLoanRequest(lang: String) = translations((CommandName.LOCK_LOAN.toString,lang))
  def getPayeesCommand(lang: String) = translations((CommandName.PAYEES.toString,lang))
  def getPeersCommand(lang: String) = translations((CommandName.PEERS.toString,lang))
  def getRateCommand(lang: String) = translations((CommandName.RATE.toString,lang))
  def getRestoreCommand(lang: String) = translations((CommandName.RESTORE.toString,lang))
  def getRescanCommand(lang: String) = translations((CommandName.RESCAN.toString,lang))
  def getSpanishLanguageCommand(lang: String) = translations((CommandName.SPANISH_LANG.toString,lang))
  def getSeedCommand(lang: String) = translations((CommandName.SEED.toString,lang))
  //def getSwapsCommand(lang: String) = translations((CommandName.SWAPS.toString,lang))
  def getSweepCommand(lang: String) = translations((CommandName.SWEEP.toString,lang))
  def getSettingsCommand(lang: String) = translations((CommandName.SETTINGS.toString,lang))
  def getSpendCommand(lang: String) = translations((CommandName.SPEND.toString,lang))
  def getTransactionsCommand(lang: String) = translations((CommandName.TRANSACTIONS.toString,lang))
  def getViewScriptCommand(lang: String) = translations((CommandName.VIEWSCRIPT.toString,lang))
  def getWhoamiCommand(lang: String) = translations((CommandName.WHOAMI.toString,lang))

  // Notification: 🔔
  // Sign: 🖊"BCH" -> "https://blockchair.com/bitcoin-cash/transaction/TXHASH"
  // Balance: ⚖️

  def getTransactionLink(coin: String, tx: Transaction): String = {
    val baseurl = Map(
      "BSV" -> "https://blockchair.com/bitcoin-sv/transaction/TXHASH",
      "tBCH" -> "https://explorer.bitcoin.com/tbch-schnorr/tx/TXHASH",
      //"tBCH" -> "https://explorer.bitcoin.com/tbch/tx/TXHASH",
      "BCH" -> "https://explorer.bitcoin.com/bch/tx/TXHASH",
      "BTC" -> "https://bitaps.com/TXHASH",
        //"https://blockchair.com/bitcoin/transaction/TXHASH",
      "tBTC" -> "https://tbtc.bitaps.com/TXHASH"
        //"https://live.blockcypher.com/btc-testnet/tx/TXHASH"
    )
    val defaulturl = s"https://blockchair.com/bitcoin-sv/transaction/TXHASH"
    baseurl.get(coin)
      .getOrElse(defaulturl)
      .replace("TXHASH",tx.getHashAsString)
    //s"[${tx.getHashAsString.take(6)}](${url})"
  }

  def getEnterAddressAsk(lang: String): String = "Enter payee's address, "

  def getSpendAmountAsk(lang: String): String = "Enter spend amount, "

  def getLockAmountAsk(lang: String): String = "Enter lock amount, "

  def getAskNewPayeeAddress(lang: String): String = "Enter payee's address. "

  def getAskNewPayeeName(lang: String): String = "Enter payee's name."

  def getP2PKHReceiveAddress(wallet: Wallet, tx: Transaction): Option[Address]
    = {
    val myOutputs = tx.getOutputs.find(_.isMineOrWatched(wallet))
    myOutputs
      .filter(_.getScriptPubKey.isSentToAddress)
      .map(_.getScriptPubKey.getToAddress(wallet.getParams, true))
  }

  def isBIP47NotificationAddress(kit: BIP47AppKit, address: Address): Boolean = {
    val notificationAddress = kit.getAccount(0).getNotificationAddress
    address.equals(notificationAddress)
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
      refundPubKey, swapPubKey, SwapUtils.REFUND_TIME_NEEDED)

    val validP2SH = ScriptPattern.isPayToScriptHash(redeemScript)

    val p2shAddress : Address =
      ScriptBuilder.createP2SHOutputScript(redeemScript)
        .getToAddress(kit.getParams)

    println(s"Adding watch script: ${p2shAddress}")
    kit.getvWallet().addWatchedScripts(List(redeemScript))

    println(s"Adding watch: ${p2shAddress}")

    kit.getvWallet().addWatchedAddress(p2shAddress)

    println("TODO: Rescanning blockchain...")
  }

  def getCoinsReceivedNotification(ownerId: Int,
                                   coin: String,
                                   kit: BIP47AppKit,
                                   tx: Transaction,
                                   prevBalance: Coin,
                                   newBalance: Coin,
                                   lang: String): SendMessage = {

    println("******************* BEGIN RECEIVE NOTIFICATION ********************")

    // is change? dont be so verbose
    val valueFromMe = tx.getValueSentFromMe(kit.getvWallet())
    if (valueFromMe.isPositive) {
      println(s"Ignoring change value: ${formatAmount(valueFromMe, coin)}")

      // if it's refund, don't return
      val chunks = tx.getInput(0).getScriptSig.getChunks
      if (chunks.length <= 2)
        return null
      else {
        if (!chunks.get(12).isPushData)
          return null
        else {
          val isMine = chunks.get(12).data.deep == kit.getAccount(0).keyAt(0).getPubKey.deep
          if (!isMine)
            return null;
        }
      }
    }

    val wallet = kit.getvWallet()
    val notification = new SendMessage()
    notification.enableMarkdown(true)

    println(s"My payment code: ${kit.getPaymentCode}")

    val receiveAddr = getP2PKHReceiveAddress(wallet, tx)
    val isNotificationTransaction =
      receiveAddr.fold(false)(isBIP47NotificationAddress(kit,_))
    val channel = receiveAddr.flatMap( getBIP47TxChannel(_, kit, tx))

    var buttonRow : mutable.Buffer[KeyboardI18NButton] = mutable.Buffer(
      (Texts.OPEN_LINK, Left(getTransactionLink(coin, tx))),
      (Texts.TX_SET_MEMO, Right(CallbackName.SET_MEMO))
    )

    var text = ""
    if (isNotificationTransaction) {
      text += s"**${translations(Texts.INCOMING_NTX.toString, lang)}**"
      text += s"\nSender's payment code: ${kit.getPaymentCodeInNotificationTransaction(tx)}"
      buttonRow.append((Texts.ADD_AS_PAYEE, Right(CallbackName.ADD_SENDER_AS_PAYEE)))
    } else {
      text += s"**${translations(Texts.INCOMING_TX.toString, lang)}**"
    }

    text += s"\n${translations(Texts.TX_HASH.toString, lang)}: ${tx.getHashAsString.take(6)}"
    text += s"\n${translations(Texts.VALUE_SENT_TO_ME.toString,lang)}: ${formatAmount(tx.getValueSentToMe(wallet), coin)}"

    if (channel.isDefined) {

      val opret = tx.getOutputs.find(_.getScriptPubKey.isOpReturn)

      val limitSwap : Option[LimitSwap]
        = opret.flatMap(extractOpReturnData).flatMap(SwapUtils.maybeLimitSwap)

      if (limitSwap.isDefined) {
        val (wantCoin, wantPrice, wantLocktime, wantSecretHash) = limitSwap.get
        val inlineKeyboard = get_limit_swap_inline_keyboard(lang)
        notification.setReplyMarkup(inlineKeyboard)
        println(s"Detected limit swap: ${limitSwap.get}")

        text += s"\nWants price: ${wantPrice}"
        text += s"\nWants coin: ${wantCoin}"
        text += s"\nWants Locktime: ${wantLocktime}"
        text += s"\nWants secret hash: ${wantSecretHash}"

        watchLimitSwap(kit, channel.get, limitSwap.get._4)
        save_db(USER_HTLC, ownerId, wantSecretHash.toString, BSONDocument(
          "limit_tx" -> tx.getHashAsString,
          "want_coin" -> BSONString(wantCoin),
          "want_price" -> BSONDouble(wantPrice),
          "want_locktime" -> BSONLong(wantLocktime),
          "swap_payee" -> BSONString(channel.get.getPaymentCode)
        ))

      } else {
        println("Could not detect limit swap")
      }
    }

    // Received a swap that is reedemable?
    val swapRedeemable : Option[RedeemableSwap] = tx.getOutputs.flatMap(out =>
      SwapUtils.maybeRedeemableSwap(out.getScriptPubKey)).headOption
    if (swapRedeemable.isDefined) {
      val (hash, whoFunded, whoSwaps, locktime) = swapRedeemable.get

      // owner can sweep when it has the secret, otherwise offer to counter lock
      val secret_value = get_collection_string_value(
        USER_HTLC, ownerId, "swap_secret", data_key = s"${hash.toString}")

      if (false == secret_value.isDefined)
        buttonRow += ((Texts.HTLC_OFFER, Right(CallbackName.SWAP_COUNTER_LOCK)))
      else
        buttonRow += ((Texts.HTLC_SWEEP, Right(CallbackName.HTLC_DO_SWEEP)))
    }

    // tx is incoming, because payer did sweep?
    if (receiveAddr.isDefined) {
      println("Checking for sweep")

      val txInputs = tx.getInputs.filter(
        input =>  input.getOutpoint!=null
        && input.getConnectedOutput != null
      )
      for (input <- txInputs) {
        val outpoint = input.getOutpoint
        val connectedOutput = outpoint.getConnectedOutput
        val txBeingSpent = connectedOutput.getParentTransaction

        println(s"TX BEING SPENT: ${txBeingSpent.getHashAsString}")

        val script = connectedOutput.getScriptPubKey
        val chunks = script.getChunks

        // The input script (3 chunks) has the 20 bytes secret
        // in position 1
        var secretBytes = Array[Byte]()

        val inChunks = input.getScriptSig.getChunks
        if (chunks.length == 3 && chunks.get(1).isPushData) {
          secretBytes = chunks.get(1).data
          if (secretBytes.size == 20) {
            println("Found secret")
          }
        }

        // find the hash based on the tx being spent.
        val secretHash = Sha256Hash.of(secretBytes)

        val secretHashCounterTx = get_collection_string_value(
          collection = USER_HTLC,
          who = ownerId,
          key = "countertx",
          data_key = secretHash.toString
        )

        val secretHashBaseTx = get_collection_string_value(
          collection = USER_HTLC,
          who = ownerId,
          key = "basetx",
          data_key = secretHash.toString
        )

        println(s"SECRET HASH COUNTER TX: ${secretHashCounterTx}")
        println(s"SECRET HASH BASE TX: ${secretHashBaseTx}")

        println(s"MATCHES BASE? ${secretHashBaseTx.toString.equals(txBeingSpent.toString)}")
        println(s"MATCHES COUNTER? ${secretHashCounterTx.toString.equals(txBeingSpent.toString)}")

        // the sweeper has taken our counter lock.
        // find the original lock tx
        if (secretBytes.nonEmpty && chunks.length > 12 && script.getChunks.get(10).isPushData) {
          val refundKey = script.getChunks.get(10).data
          val notificationPubkey = kit.getAccount(0).keyAt(0).getPubKey

          if (refundKey.deep == notificationPubkey.deep) {
            /*val redeemableSwap = SwapUtils.maybeRedeemableSwap(script)
            val (secretHash,whoFunded,whoSwaps,locktime) = redeemableSwap.get

            // we have the hash, what's the secret?
            val secret_hex = get_collection_string_value(
              USER_HTLC, ownerId, "swap_secret", secretHash.toString)

            val swap_payee = get_collection_string_value(
              USER_HTLC, ownerId, "swap_payee", secretHash.toString)

            val secret = Utils.HEX.decode(secret_hex.get)
            val swap = new Swap(kit.getParams) //TODO: get the right params
            val sweepScript = swap.getSwapInputScript(script, secret)

            val outpoint = new TransactionOutPoint(ps, utxo.getIndex, utxo.getParentTransactionHash)
            val txInput = new TransactionInput(ps, htlcTx, sweepScript.getProgram, outpoint, utxo.getValue)

            // 512 seconds
            //txInput.setSequenceNumber(2L)
            val spendTx = new Transaction(ps)
            spendTx.addInput(txInput)

            val feePerKb = if (!wallet.getParams.getUseForkId)
              Transaction.DEFAULT_TX_FEE
            else
              Transaction.BCC_DEFAULT_TX_FEE

            if (swap_payee.isDefined) {
              val channel = wallet.getBip47MetaForPaymentCode(swap_payee.get)
              val payeeAddress = nextAddress(wallet, channel)
              spendTx.addOutput(utxo.getValue, payeeAddress)
            } else {
              spendTx.addOutput(utxo.getValue, wallet.getCurrentAddress)
            }

            val feeAmount = feePerKb.div(1000).multiply(spendTx.getMessageSize);
            spendTx.getOutput(0).setValue(spendTx.getOutput(0).getValue.minus(feeAmount))

            spendTx.verify()
            spendTx.setVersion(2)

            val sighash = spendTx.hashForSignature(0, utxo.getScriptPubKey.getProgram, Transaction.SigHash.ALL.byteValue())
            val signKey = wallet.getAccount(0).keyAt(0)
            val mySig : ECKey.ECDSASignature = signKey.sign(sighash)
            val txSig = new TransactionSignature(mySig, SigHash.ALL, false)

            val reinputScript = new ScriptBuilder()
              .data(txSig.encodeToBitcoin())
              .data(secret)
              .smallNum(1).build()

            try {
              reinputScript.correctlySpends(spendTx, 0,
                utxo.getScriptPubKey, Script.ALL_VERIFY_FLAGS)
            } catch {
              case e: Throwable => println(s"Fails to spend: ${e.printStackTrace()}")
            }

            spendTx.clearInputs()
            val txInput2 = new TransactionInput(ps, htlcTx, reinputScript.getProgram, outpoint, utxo.getValue)
            txInput.setScriptSig(reinputScript)
            spendTx.addInput(txInput2)
            assert(spendTx.getInputs.size == 1)

            assert(spendTx.getFee.isPositive)
            println(Utils.HEX.encode(spendTx.bitcoinSerialize()))
            wallet.broadcastTransaction(spendTx)*/
          }
        }

      }
    }

    val rows = List(buttonRow.toList)
    notification.setReplyMarkup(make_inline_keyboard(rows, lang))
    println("******************* END RECEIVE NOTIFICATION ********************")

    notification.setText(text)
    return notification
  }

  def getCoinsSentNotification(coin: String, wallet: Wallet, tx: Transaction, prevBalance: Coin, newBalance: Coin, lang: String): SendMessage= {
    println("******************* BEGIN SENT NOTIFICATION ********************")
    println(s"TX: ${tx.getHashAsString}")
    val notification = new SendMessage()
    notification.enableMarkdown(true)

    var text = ""

    println("Outgoing tx values... ")
    println(s"Value from me: ${formatAmount(tx.getValueSentFromMe(wallet), coin)}")
    println(s"Value to me: ${formatAmount(tx.getValueSentToMe(wallet), coin)}")
    println(s"Value : ${formatAmount(tx.getValue(wallet), coin)}")

    val total = tx.getValueSentFromMe(wallet).add(tx.getFee).minus(tx.getValueSentToMe(wallet))

    println(s"Total : ${formatAmount(total, coin)}")

    text += s"${translations((Texts.OUTGOING_TX.toString, lang))}"
    text += s"\n${translations(Texts.TX_HASH.toString, lang)}: ${tx.getHashAsString.take(6)}"
    text += s"\n${translations(Texts.VALUE_SENT_FROM_ME.toString, lang)}: " +
      s"${formatAmount(tx.getValue(wallet), coin)}"

    val url = getTransactionLink(coin, tx)
    val buttons = mutable.ListBuffer[(Texts.Value,Either[String,CallbackName.Value])](
      (Texts.OPEN_LINK, Left(url))
    )
    val rows = List(buttons.toList)
    val keyboard = make_inline_keyboard(rows, lang)

    notification.setReplyMarkup(keyboard)
    notification.setText(text)
    println("******************* END SENT NOTIFICATION ********************")
    return notification
  }
  import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup
  import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.KeyboardRow

  def make_inline_keyboard
    (buttonRows: List[List[(Texts.Value, Either[String, CallbackName.Value])]],
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

  def make_options_keyboard(xs: List[String], ncols : Int): ReplyKeyboardMarkup = {
    //println(s"Making options: ${xs.mkString(",")} in ${ncols} columns")
    val replyKeyboardMarkup = new ReplyKeyboardMarkup
    replyKeyboardMarkup.setSelective(true)
    replyKeyboardMarkup.setResizeKeyboard(true)
    replyKeyboardMarkup.setOneTimeKeyboard(false)

    val keyboard : java.util.List[KeyboardRow] = new java.util.ArrayList()
    var nextRow : KeyboardRow = null

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

  // format helpers
  def formatAmount(coin: Coin, coinName: String): String = {
    coin.toFriendlyString.replace("BTC", coinName)
  }

  def formatAddress(address: Address, coinName: String): String = {
    val isCash = coinName.contains("BCH") || coinName.contains("BSV")

    if (isCash && !(coinName.contains("BSV"))) {
      new CashAddress(address).encode()
    } else
      address.toString
  }

  def formatBalance(wallet: BIP47AppKit): String = {
    formatAmount(wallet.getBalance, wallet.getCoinName)
      .replace(wallet.getCoinName, s"*${wallet.getCoinName}*")
  }

  // 0. Main menu
  def get_main_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options =
      List(
        getWalletsCommand(lang),
        getSpendCommand(lang),
        getHTLCListCommand(lang),
        getSettingsCommand(lang)
      )
    make_options_keyboard(options, 2)
  }

  def get_swaps_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options =
      List(
        getLockSwapCommand(lang),
        getHTLCListCommand(lang),
        getBackCommand(lang)
      )
    make_options_keyboard(options, 3)
  }

  // help functions
  def get_back_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    make_options_keyboard(List(getBackCommand(lang)), 2)
  }

  def get_cancel_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    make_options_keyboard(List(getCancelCommand(lang)), 2)
  }

  // 1. Wallet menu
  def get_wallet_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options = List(
      getWalletsCommand(lang),
      getAddressListCommand(lang),
      getTransactionsCommand(lang),
      getSpendCommand(lang),
      getLockCommand(lang),
      getPeersCommand(lang),
      getBackCommand(lang))
    make_options_keyboard(options, 2)
  }

  def faucetable_coins(): Seq[String] = {
    val adminExists = userDir(adminId).exists()
    val adminHasWallets = wallMap.contains(adminId)
    if (false == adminExists || false == adminHasWallets) {
      throw new CatchError(Errata.FAUCET_NEEDS_ADMIN)
    }

    val faucetCoins = mutable.Set[String]()
    val adminWallets : UserWallets = wallMap(adminId)
    for (wall <- adminWallets){
      // only test bitcoins
      if (wall._1.startsWith("t")) {
        if (wall._2.getBalance.isGreaterThan(faucetCoin)) {
          faucetCoins += wall._1
        }
      }
    }
    faucetCoins.toSeq
  }

  def spendable_coins(uid: Int): Seq[String] = {
    val options : ListBuffer[String] = ListBuffer()
    for (w <- wallMap.getOrElse(uid, mutable.Map())) {
      val balance = w._2.getBalance
      val network = w._2.getParams
      val minBalance = network.getMinNonDustOutput.add(network.getReferenceDefaultMinTxFee)

      if (balance.isGreaterThan(minBalance)) {
        options += w._1
      }
    }
    options
  }

  // 2. Spend
  def get_spend_coin_menu_keyboard(uid: Int, lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer()
    options ++= spendable_coins(uid)
    options += getBackCommand(lang)
    make_options_keyboard(options.toList, 2)
  }

  def get_faucet_coin_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer()
    options ++= faucetable_coins()
    options += getBackCommand(lang)
    make_options_keyboard(options.toList, 3)
  }

  // 2.2 Enter amount
  def get_spend_amount_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    //val balance = kit.getvWallet()
    val options = List("1%", "5%", "10%", "20%", "50%", "100%", getCancelCommand(lang))
    make_options_keyboard(options, 3)
  }

  // 2.3 Confirm Spend
  def get_broadcast_tx_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options = List(getEncodeCommand(lang), getBroadcastCommand(lang), getCancelCommand(lang))
    make_options_keyboard(options, 3)
  }

  def get_broadcast_contract_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options = List(getViewScriptCommand(lang), getEncodeCommand(lang), getBroadcastCommand(lang), getCancelCommand(lang))
    make_options_keyboard(options, 3)
  }

  // contact/recipient functions
  /*def get_add_payee_menu_keyboard(uid: Int, lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer(getBackCommand(lang))
    options += getAddPayeeCommand(lang)
    options += getCancelCommand(lang)
    make_options_keyboard(options.toList, 2)
  }*/

  def get_all_coins_menu_keyboard(uid: Int, lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer()
    val coins : List[String] = wallMap.get(uid).fold[List[String]](List())(map => map.keys.toList)
    options ++= coins
    options += getBackCommand(lang)
    make_options_keyboard(options.toList, 3)
  }

  def payees(m: Message): List[(String,String)] = {
    val uid = m.getFrom.getId
    val bson : Option[BSONValue] =
      get_collection_value(USER_PAYEE_COLLECTION, uid, "data", BSONArray())

    val payeeList: BSONArray = bson.get.asInstanceOf[BSONArray]
    val ret = mutable.ListBuffer[(String,String)]()
    println(s"Payees: ${payeeList}")

    for (BSONElement(i,p) <- payeeList.elements) {
      p match {
        case d: BSONDocument => {
          val payee_name : BSONString = d.get("payee_name").get.asInstanceOf[BSONString]
          val payee_address : BSONString = d.get("payee_address").get.asInstanceOf[BSONString]
          ret += ((payee_name.value, payee_address.value))
        }
        case y => println(s"MATCHING ERROR: ${y}")
      }
    }
    ret.toList
  }

  def get_choose_payee_menu_keyboard(m: Message, lang: String): ReplyKeyboardMarkup = {
    val uid = m.getFrom.getId
    val options : ListBuffer[String] = ListBuffer()

    for ((payee_name,payee_address) <- payees(m)) {
      options += payee_name
    }
    options += getAddPayeeCommand(lang)
    options += getBackCommand(lang)
    options += getCancelCommand(lang)
    make_options_keyboard(options.toList, 1)
  }

  // 3. Lock
  def get_lock_coin_menu_keyboard(uid: Int, lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer()
    options ++= spendable_coins(uid)
    options += getBackCommand(lang)
    make_options_keyboard(options.toList, 2)
  }

  // 3.1 Lock coin
  def get_lock_scripts_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer()
    options += getLockSwapCommand(lang)
    options += getCancelCommand(lang)
    make_options_keyboard(options.toList, 2)
  }

  def get_coin_menu_keyboard(uid: Int, lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer()
    options ++= wallMap.getOrElse(uid, mutable.Map()).keySet.toList
    options += getCancelCommand(lang)
    make_options_keyboard(options.toList, 2)
  }

  def get_limit_swap_inline_keyboard(lang: String): InlineKeyboardMarkup = {
    val opts = List((Texts.SWAP_FILL, Right(CallbackName.SWAP_LIMIT_FILL)))
    val rows = List(opts)
    return make_inline_keyboard(rows, lang)
  }

  def get_htlc_list_inline_keyboard(htlc: HTLCFromTx,
                                    url: String,
                                    lang: String,
                                    current: Int,
                                    total: Int): InlineKeyboardMarkup = {

    if (total == 0)
      return make_inline_keyboard(List(), lang)

    val openBtn = (Texts.OPEN_LINK, Left(url))

    val sweepBtn = (Texts.HTLC_SWEEP, Right(CallbackName.HTLC_DO_SWEEP))
    val refundBtn = (Texts.HTLC_REFUND, Right(CallbackName.HTLC_DO_REFUND))
    val viewBtn = (Texts.VIEW_CONTRACT, Right(CallbackName.VIEW_HTLC))
    val nextBtn = (Texts.NEXT, Right(CallbackName.NEXT_HTLC))
    val prevBtn = (Texts.PREV, Right(CallbackName.PREV_HTLC))

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
    return make_inline_keyboard(rows, lang)
  }

  // 4. Settings
  def get_settings_menu_keyboard(uid: Int, lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer()
    options += getWhoamiCommand(lang)
    options += getSeedCommand(lang)
    options += getLanguageCommand(lang)
    options += getInfoCommand(lang)
    options += getBackCommand(lang)
    make_options_keyboard(options.toList, 2)
  }

  def get_language_menu_keyboard(lang: String): ReplyKeyboardMarkup = {
    val options : ListBuffer[String] = ListBuffer()
    options += getEnglishLanguageCommand(lang)
    options += getSpanishLanguageCommand(lang)
    options += getCancelCommand(lang)
    make_options_keyboard(options.toList, 2)
  }

  //getPayeesCommand(lang)
  def get_language(m: Message): String =

    get_collection_value(LANGUAGE_COLLECTION, m.getFrom.getId, "lang") match {
      case Some(BSONString(value)) => value
      case _ => get_collection_value(LANGUAGE_COLLECTION, m.getChatId.toInt, "lang") match {
        case Some(BSONString(value)) => value
        case _ => "es"
      }
    }


  def set_language(who: Who, lang: String) = save_db(LANGUAGE_COLLECTION, who, "data", document("lang" -> lang))

  def get_meta_string(who: Who, key: String) : Option[String] =
    get_collection_value(USER_META_COLLECTION, who, key).flatMap(optCoin => optCoin match {
      case BSONString(value) => Some(value)
      case ext => {
        println(s"get_meta_string ${key} Match error: now got: ${ext}")
        None
      }
    }) match {
      case None => {println(s"WARNING: Missing meta: ${key}"); None}
      case x => x
    }

  def get_meta_string(m: Message, key: String): Option[String] = get_meta_string(m.getFrom.getId, key)

  def get_meta_long(who: Who, key: String) : Option[Long] =
    get_collection_value(USER_META_COLLECTION, who, key).flatMap(optCoin => optCoin match {
      case BSONLong(value) => Some(value)
      case ext => {
        println(s"get_meta_long Match error: now got: ${ext}")
        None
      }
    })

  def get_meta_long(m: Message, key: String): Option[Long] = get_meta_long(m.getFrom.getId, key)

  def get_state(who: Who) : StateName.Value = {
    //import reactivemongo.bson.Macros
    val st : BSONValue = get_collection_value(USER_STATE_COLLECTION, who, "state", BSONString(STARTSTATE.toString())).get
    st match {
      case BSONString(value) => StateName.withName(value)
      case _ => throw new Exception()
    }

  }

  def save_db(collection: CollectionName.Value, who: Who, key: String, data: BSONValue): Unit = {
    val selector = BSONDocument("who" -> BSONInteger(who)) //document("who" -> who)
    val modifier = BSONDocument("$set" -> BSONDocument(key->data))
    val upsert : Future[reactivemongo.api.commands.UpdateWriteResult] = col(collection)
      .flatMap(_.update(selector, modifier, upsert = true))
    val res = Await.ready(upsert, 2 seconds)
  }

  def insert_db(collection: CollectionName.Value, who: Who, key: String, data: BSONValue): Unit = {
    val selector = BSONDocument("who" -> BSONInteger(who)) //document("who" -> who)
    val modifier = BSONDocument("$push" -> BSONDocument(key->data))
    val upsert : Future[reactivemongo.api.commands.UpdateWriteResult] = col(collection)
      .flatMap(_.update(selector, modifier, upsert = true))
    val res = Await.ready(upsert, 2 seconds)
  }

  def save_meta(m: Message, key : String, value: BSONValue) = {
    println(s"Saving ${key} ... ") //=> ${value}")
    save_db(USER_META_COLLECTION, m.getFrom.getId, s"data.${key}", value)
  }

  def clean_meta(m: Message) =
    save_db(USER_META_COLLECTION, m.getFrom.getId, "data", document())

  def save_state(m: Message, state: StateName.Value): Unit = {
    val who = m.getFrom.getId
    println(s"Saving state of ${who} to ${state.toString()}")
    val v : BSONDocument = document("state" -> state.toString) //Document("state" -> state.toString()
    save_db(USER_STATE_COLLECTION, who, "data", v)
  }

  def editTextMessage(bot: TelegramLongPollingBot,
                      message: Message,
                      inlineKeyboard: InlineKeyboardMarkup,
                      text: String
                     ): Unit = {

    val new_message = new EditMessageText
    new_message.setChatId(message.getChatId)
    new_message.setMessageId(message.getMessageId)
    new_message.setText(text)
    new_message.setReplyMarkup(inlineKeyboard)
    bot.execute[Serializable, BotApiMethod[Serializable]](new_message)
  }

  def recv_call(update: Update, bot: TelegramLongPollingBot): Unit = {

    println("******************* BEGIN CALL ********************")

    assert(update.hasCallbackQuery)
    val query = update.getCallbackQuery
    val callbackCmd = CallbackName.withName(query.getData)
    println(s"Callback: ${callbackCmd}")
    val message = query.getMessage

    val currentText = message.getText
    val ownerId = query.getFrom.getId
    val lang = get_language(message)

    if (false == wallMap.contains(ownerId)) {
      println("Automatically starting wallets ...")
      startWallets(message, bot, lang)
    }

    val wallets = wallMap.getOrElse(
      ownerId, mutable.Map()).values.toList

    val txPattern = s".*${translations(Texts.TX_HASH.toString, lang).toLowerCase}: ([A-Za-z0-9]+) ?.*".r
    val normalizedText = currentText.toLowerCase.replace("\n", " ")

    val maybeHTLCTx : Option[HTLCFromTx] = normalizedText match {

      case txPattern(txHash) => {
        val htlcList = listHTLCS(wallets)
        val i = htlcList
          .map(_.getHashAsString).indexWhere(_.startsWith(txHash))
        val tx = htlcList(i)
        val kit = wallets.find(_.getvWallet().getTransaction(tx.getHash) != null)
        val ps = kit.get.getParams
        val utxo = tx.getOutputs.find(out =>
          SwapUtils.maybeRedeemableSwap(out.getScriptPubKey).isDefined
        ).get
        val redeemScript = utxo.getScriptPubKey
        Some((ps, kit.get, tx, utxo, redeemScript))
      }
      case _ => None
    }

    callbackCmd match {
      case CallbackName.SWAP_LIMIT_FILL => {

      }

      case CallbackName.OPEN_TX_LINK => {
        println("Clicked OPEN TX LINK")
      }

      case CallbackName.HTLC_DO_REFUND => maybeHTLCTx match {

        case None => println("WARN: No htlc tx")
        case Some((ps, wallet, tx, utxo, htlc)) => {

          val swap = new Swap(ps)
          val refundScript = swap.getRefundInputScript(utxo.getScriptPubKey)

          val outpoint = new TransactionOutPoint(ps, utxo.getIndex, utxo.getParentTransactionHash)
          val refundInput = new TransactionInput(ps, tx, refundScript.getProgram, outpoint, utxo.getValue)

          val refundTx = new Transaction(ps)
          refundTx.addInput(refundInput)
          refundTx.addOutput(utxo.getValue.div(2), wallet.getCurrentAddress)
          refundTx.verify()
          refundTx.setVersion(2)

          refundInput.setSequenceNumber(2L)
          refundInput.setScriptSig(refundScript)

          val sighash = refundTx.hashForSignature(0, utxo.getScriptPubKey.getProgram, Transaction.SigHash.ALL.byteValue())
          val signKey = wallet.getAccount(0).keyAt(0)
          val mySig : ECKey.ECDSASignature = signKey.sign(sighash)
          val txSig = new TransactionSignature(mySig, SigHash.ALL, false)
          val signedRefundScript = new ScriptBuilder()
            .data(txSig.encodeToBitcoin())
            .smallNum(0)
            .build()

          try {
            signedRefundScript.correctlySpends(refundTx, 0,
              utxo.getScriptPubKey, Script.ALL_VERIFY_FLAGS)
          } catch {
            case e: Throwable => println(s"Fails to spend: ${e.printStackTrace()}")
          }

          println(s"My key: ${Utils.HEX.encode(signKey.getPubKey)}")
          println(s"Refund key: ${Utils.HEX.encode(utxo.getScriptPubKey.getChunks.get(10).data)}")
          println(s"SWAP key: ${Utils.HEX.encode(utxo.getScriptPubKey.getChunks.get(4).data)}")

          refundTx.clearInputs()
          val signedInput = new TransactionInput(ps, tx, signedRefundScript.getProgram, outpoint, utxo.getValue)
          signedInput.setSequenceNumber(2L)
          signedInput.setScriptSig(signedRefundScript)
          refundTx.addInput(signedInput)

          assert(refundTx.getInputs.size == 1)
          println("SPEND SERIALIZE POST:")
          println(Utils.HEX.encode(refundTx.bitcoinSerialize()))

          wallet.broadcastTransaction(refundTx)

          val msg = replyTextMessage(message.getChatId, message.getMessageId,
            Utils.HEX.encode(refundTx.bitcoinSerialize()))

          bot.execute[Message, BotApiMethod[Message]](msg)
        }
      }

      case CallbackName.VIEW_HTLC => maybeHTLCTx match {
        case None => println("WARN: No htlc tx")
        case Some((ps, wallet, tx, utxo, htlc)) => {
          val text = utxo.getScriptPubKey.toString
            //Utils.HEX.encode(tx.bitcoinSerialize())
          replyTextMessage(bot, message.getChatId, message.getMessageId, text)
        }
      }

      case CallbackName.NEXT_HTLC
           | CallbackName.PREV_HTLC => maybeHTLCTx match {
        case None => println("WARN: No htlc tx")
        case Some((ps, wallet, tx, utxo, htlc)) => {

          val htlcList = listHTLCS(wallets)
          val currentIndex = htlcList
            .map(_.getHashAsString).indexWhere(_.startsWith(tx.getHashAsString))

          val nextIndex = if (callbackCmd.equals(CallbackName.PREV_HTLC))
            currentIndex - 1 else currentIndex + 1

          val next = htlcList(nextIndex)
          val newText = showHTLC(next, wallet, nextIndex, htlcList.size, lang)

          val inlineKeyboard = get_htlc_list_inline_keyboard(
            maybeHTLCTx.get, getTransactionLink(wallet.getCoinName, next), lang, nextIndex, htlcList.size)

          assert(!newText.equals(message.getText))
          editTextMessage(bot, message, inlineKeyboard, newText)
        }
      }

      // Someone locked funds for us (tx is a redeemable swap)
      // Lock our funds for them (send them a reedemable swap).
      case CallbackName.SWAP_COUNTER_LOCK => maybeHTLCTx match {
        case None => println("WARN: No htlc tx")
        case Some((ps, wallet, tx, utxo, htlc)) => {
          println(s"Found tx: ${tx.getHashAsString}")
          // get redeemable swap
          val redeemableSwap = SwapUtils.maybeRedeemableSwap(htlc)
          val (secretHash,whoFunded,whoSwaps,locktime) = redeemableSwap.get

          // find where to lock.
          val limitSwap : Option[BSONDocument] = get_collection_doc_value(
            USER_HTLC, ownerId, data_key = secretHash.toString
          )

          val swap_coin = limitSwap.get.get("want_coin").get.asInstanceOf[BSONString]
          val swap_payee = limitSwap.get.get("swap_payee").get.asInstanceOf[BSONString]
          val swap_price = limitSwap.get.get("want_price").get.asInstanceOf[BSONDouble]

          val want_network = networks(swap_coin.value)
          println(s"WANT NETWORK: ${want_network.toString}")
          val swap = new Swap(want_network)

          val payee_paycode = new BIP47PaymentCode(swap_payee.value)
          val swapPubKey = payee_paycode.derivePubKeyAt(want_network, 0)
          val refundPubKey = wallet.getAccount(0).keyAt(0).getPubKey;

          val htlc_script = swap.getRedeemScript(secretHash,
            refundPubKey, swapPubKey, SwapUtils.REFUND_TIME_NEEDED)

          // Price :: Want/Have  (e.g. received 'lockedHave' :: tBTC)
          // e.g. 20 :: tBCH/tBTC
          // => Want = Price*lockedHave
          // e.g. tBCH = tBTC*20
          val lockedHave = utxo.getValue
          val Price = swap_price.value

          val wantAmount = lockedHave.value*Price
          val lockedWant = Coin.valueOf(wantAmount.toLong)

          val htlc_tx = new Transaction(want_network)
          htlc_tx.addOutput(lockedWant, htlc_script)

          println("Broadcasting ...")

          val htlc_tx_req = SendRequest.forTx(htlc_tx)
          wallet.getvWallet().completeTx(htlc_tx_req)
          htlc_tx.verify()

          println(s"Saving base/counter txs in secret " +
            s"hash ${secretHash.toString}")
          save_db(
            collection = USER_HTLC,
            who = message.getChatId.toInt, //fix: chat id as user
            key = secretHash.toString + ".basetx",
            data = BSONString(tx.getHashAsString)
          )

          save_db(
            collection = USER_HTLC,
            who = message.getFrom.getId,
            key = secretHash.toString + ".countertx",
            data = BSONString(htlc_tx.getHashAsString)
          )

          println(s"Broadcasting tx: ${htlc_tx.getHash}")
          wallet.broadcastTransaction(htlc_tx)
        }
      }

      // The generator of the secret decides to sweep
      // Sweep, i.e. spend the tx locked by the counter party using our secret
      case CallbackName.HTLC_DO_SWEEP => maybeHTLCTx match {
        case None => println("WARN: No htlc tx")
        case Some((ps, wallet, htlcTx, utxo, htlc)) => {
          // get redeemable swap
          val redeemableSwap = SwapUtils.maybeRedeemableSwap(htlc)
          val (secretHash,whoFunded,whoSwaps,locktime) = redeemableSwap.get

          // we have the hash, what's the secret?
          val secret_hex = get_collection_string_value(
            USER_HTLC, ownerId, "swap_secret", secretHash.toString)

          val swap_payee = get_collection_string_value(
            USER_HTLC, ownerId, "swap_payee", secretHash.toString)

          val secret = Utils.HEX.decode(secret_hex.get)
          val swap = new Swap(ps)
          val sweepScript = swap.getSwapInputScript(htlc, secret)

          val outpoint = new TransactionOutPoint(ps, utxo.getIndex, utxo.getParentTransactionHash)
          val txInput = new TransactionInput(ps, htlcTx, sweepScript.getProgram, outpoint, utxo.getValue)

          // 512 seconds
          //txInput.setSequenceNumber(2L)
          val spendTx = new Transaction(ps)
          spendTx.addInput(txInput)

          val feePerKb = if (!wallet.getParams.getUseForkId)
            Transaction.DEFAULT_TX_FEE
          else
            Transaction.BCC_DEFAULT_TX_FEE

          if (swap_payee.isDefined) {
            val channel = wallet.getBip47MetaForPaymentCode(swap_payee.get)
            val payeeAddress = nextAddress(wallet, channel)
            spendTx.addOutput(utxo.getValue, payeeAddress)
          } else {
            spendTx.addOutput(utxo.getValue, wallet.getCurrentAddress)
          }

          val feeAmount = feePerKb.div(1000).multiply(spendTx.getMessageSize);
          spendTx.getOutput(0).setValue(spendTx.getOutput(0).getValue.minus(feeAmount))

          spendTx.verify()
          spendTx.setVersion(2)

          val sighash = spendTx.hashForSignature(0, utxo.getScriptPubKey.getProgram, Transaction.SigHash.ALL.byteValue())
          val signKey = wallet.getAccount(0).keyAt(0)
          val mySig : ECKey.ECDSASignature = signKey.sign(sighash)
          val txSig = new TransactionSignature(mySig, SigHash.ALL, false)

          val reinputScript = new ScriptBuilder()
            .data(txSig.encodeToBitcoin())
            .data(secret)
            .smallNum(1).build()

          try {
            reinputScript.correctlySpends(spendTx, 0,
              utxo.getScriptPubKey, Script.ALL_VERIFY_FLAGS)
          } catch {
            case e: Throwable => println(s"Fails to spend: ${e.printStackTrace()}")
          }

          spendTx.clearInputs()
          val txInput2 = new TransactionInput(ps, htlcTx, reinputScript.getProgram, outpoint, utxo.getValue)
          txInput.setScriptSig(reinputScript)
          spendTx.addInput(txInput2)
          assert(spendTx.getInputs.size == 1)

          assert(spendTx.getFee.isPositive)
          println(Utils.HEX.encode(spendTx.bitcoinSerialize()))
          wallet.broadcastTransaction(spendTx)
        }
      }

      case e => {
        println(s"TODO Received callback. ${e}")
      }
    }

    println("******************* END CALL ********************")
  }

  def recv_up(update: Update, bot: TelegramLongPollingBot): Unit = {

    assert(update.hasMessage())
    val m : Message = update.getMessage()

    println("******************* BEGIN UPDATE ********************")
    if (false == update.getMessage.getText.isEmpty) {
      println(s"Received message ID: ${update.getMessage.getMessageId}")
    }

    try {
      recv_in(m, bot)
    } catch {
      case SendPhotoCatch(sendPhoto) => {
        bot.execute(sendPhoto)
      }
      case CatchError(Errata.MISSING_SPEND_COINS) => {
        val lang = get_language(m)
        val text = translations((Errata.MISSING_SPEND_COINS.toString, lang))
        val keyboard = make_options_keyboard(List(getFaucetCommand(lang), getBackCommand(lang)), 2)
        val msg = sendChooseOptionMessage(m, keyboard, lang, text)
        bot.execute[Message, BotApiMethod[Message]](msg)
      }
      case o@CatchError(error) => {

        if (error == Errata.NEEDS_BIP47_CHANNEL) {
          val (coin, kit) =
            validate_coin(m, get_meta_string(m, "coin"))

          val payee_address = get_meta_string(m, "payee_address")
          val payee_code = new BIP47PaymentCode(payee_address.get)

          save_state(m, StateName.BROADCAST_PAYEE_NTX)

          val ntx = kit.makeNotificationTransaction(payee_address.get)
          val tx = ntx.tx

          assert(ntx!=null)
          val ntxHex = Utils.HEX.encode(ntx.tx.bitcoinSerialize())

          save_meta(m, "ntxhex", BSONString(ntxHex))
          save_meta(m, "ntxhash", BSONString(ntx.tx.getHashAsString))

          val wall = kit.getvWallet()

          val acc = new BIP47Account(kit.getParams, payee_code.toString)

          val notificationAddress : Address = acc.getNotificationAddress;
          println(s"Notification address: ${notificationAddress}")
          assert(notificationAddress != null)

          val lang = get_language(m)

          val total = tx.getValue(wall)
            //tx.getValueSentFromMe(wall).add(tx.getFee).minus(tx.getValueSentToMe(wall))

          var text = ""
          text += translations((Texts.CREATE_CHANNEL_CONFIRM.toString, lang))
          text += s"\n${formatAmount(kit.getParams.getMinNonDustOutput, coin)} to ${notificationAddress.toString}" +
            s"\n------------------------" +
            s"\n${formatAmount(tx.getFee, coin)} as transaction fee" +
            s"\n------------------------" +
            s"\nTotal ${formatAmount(total, coin)}"

          val keyboard = get_broadcast_tx_menu_keyboard(lang)
          val msg = sendChooseOptionMessage(m, keyboard, lang, text)
          bot.execute[Message, BotApiMethod[Message]](msg)
        } else {
          println(s"${o.toString}. Catched error: ")
          val lang = get_language(m)
          bot.execute[Message, BotApiMethod[Message]](replyError(m, error, lang))
        }

      } case e: Throwable => {
        println(s"${e.toString}. Failed to process message: " + e.getStackTraceString)
      }
    }

    println("\n******************* END UPDATE ********************")
  }

  import scala.math.Ordering.String

  def isCommandForOther(text: String) = {
    val isSimpleCommand = text.equals("/start") || text.equals("/help") || text.equals("/stop")
    val isCommandForMe = text.equals("/start@XCSwaps_Bot") || text.equals("/help@XCSwaps_Bot") || text.equals("/stop@XCSwaps_Bot")
    text.startsWith("/") && !isSimpleCommand && !isCommandForMe
  }

  def userDir(owner: Int): java.io.File = new java.io.File(s"wallets/${run_env}/${owner}")

  def mkBIP47Wallet(bot: TelegramLongPollingBot, chatId: Long, ownerId: Int, coin: String,
                    seed: DeterministicSeed = null, lang: String): BIP47AppKit = {
    val walldir : java.io.File = userDir(ownerId)
    val network = networks(coin)
    val kit = new BIP47AppKit(coin, network, walldir, seed);

    val receiveListener: WalletCoinsReceivedEventListener = new WalletCoinsReceivedEventListener {
      override def onCoinsReceived(wallet: Wallet, transaction: Transaction, prevBalance: Coin, newBalance: Coin): Unit = {

        if (transaction.getValueSentFromMe(wallet).isPositive) {
          println(s"Ignoring (change) incoming tx ... ${transaction.getHashAsString}")
          //return
        }

        try {
          val notification = getCoinsReceivedNotification(
            ownerId, coin, kit, transaction, prevBalance, newBalance, lang)
          if (notification != null) {
            notification.setChatId(chatId)
            bot.execute[Message, BotApiMethod[Message]](notification)
          } else {
            println("WARN: null receive notification")
          }
        } catch {
          case e: Throwable => println(s"${e.toString()}. Failed to create receive notification " + e.getStackTraceString)
        }
      }
    }

    kit.getvWallet().addCoinsReceivedEventListener(receiveListener)

    val sentListener: WalletCoinsSentEventListener = new WalletCoinsSentEventListener {
      override def onCoinsSent(wallet: Wallet, transaction: Transaction, prevBalance: Coin, newBalance: Coin): Unit = {
        val notification = getCoinsSentNotification(coin, wallet, transaction, prevBalance, newBalance, lang)
        if (notification != null) {
          notification.setChatId(chatId)
          bot.execute[Message, BotApiMethod[Message]](notification)
        } else {
          println("WARN: null sent notification")
        }
      }
    }

    kit.getvWallet().addCoinsSentEventListener(sentListener)
    kit.startBlockchainDownload()
    kit
  }

  def qrFile(m: Message) = new File(
    userDir(m.getFrom.getId).getPath, "qr-code.png")

  def mkWhoamiQr(m : Message, paycode: String) :Unit = {
    import java.awt.image.BufferedImage
    import java.io.File
    import java.util.List
    import javax.imageio.ImageIO
    import io.nayuki.qrcodegen._

    // Simple operation
    val qr0 = QrCode.encodeText(paycode, QrCode.Ecc.MEDIUM)
    val img = qr0.toImage(10, 2)
    ImageIO.write(img, "png", qrFile(m))
  }

  def startWallets(m: Message, bot: TelegramLongPollingBot, lang: String): Unit = {
    if (wallMap.contains(m.getFrom.getId)) return

    val usrDir = userDir(m.getFrom.getId)
    println(s"Starting wallets in ${usrDir} ...")

    var seed : DeterministicSeed = null
    val userHasWallets = usrDir.exists()

    if (false == userHasWallets) {
      seed = new DeterministicSeed(new SecureRandom(), 256, "", System.currentTimeMillis() / 1000);
    }

    try {
      val bsv = mkBIP47Wallet(bot, m.getChatId, m.getFrom.getId, "BSV", seed, lang)
      //val bch = mkBIP47Wallet(bot, m.getChatId, m.getFrom.getId, "BCH", seed, lang)
      //val btc = mkBIP47Wallet(bot, m.getChatId, m.getFrom.getId, "BTC", seed, lang)
      val tbtc = mkBIP47Wallet(bot, m.getChatId, m.getFrom.getId, "tBTC", seed, lang)
      val tbch = mkBIP47Wallet(bot, m.getChatId, m.getFrom.getId, "tBCH", seed, lang)

      //bch.getPeerGroup.addAddress(new PeerAddress(bch.getParams, InetAddresses.forString("192.168.0.2"), 8334));
      tbtc.getPeerGroup.addAddress(new PeerAddress(tbtc.getParams, InetAddresses.forString("192.168.0.2"), 18333));
      tbch.getPeerGroup.addAddress(new PeerAddress(tbch.getParams, InetAddresses.forString("192.168.0.2"), 18334));

      val wallets : mutable.Map[String,BIP47AppKit] = mutable.Map()
      //mkWhoamiQr(m, bsv.getPaymentCode)

      wallets.put("BSV", bsv)
      //wallets.put("BCH", bch)
      //wallets.put("BTC", btc)
      wallets.put("tBTC", tbtc)
      wallets.put("tBCH", tbch)

      println(s"Created wallets: ${wallMap.keys.mkString(",")}")

      wallMap.put(m.getFrom.getId, wallets)
      save_state(m, MAINMENU)
    } catch {
      case e: Throwable => println(s"${e.toString()}. Failed to create wallets " + e.getStackTraceString)
    }
  }

  def recv_in(m: Message, bot: TelegramLongPollingBot): Unit = {

    if (isCommandForOther(m.getText())) {
      println("Warning: command for other")
      return
    }

    val lang = get_language(m)
    val incomingMsg = m.getText

    val cancelCmd = getCancelCommand(lang)
    val backCmd = getBackCommand(lang)
    val faucetCmd = getFaucetCommand(lang)

    // connect user wallets
    if (incomingMsg != "/start" && false == wallMap.contains(m.getFrom.getId)) {
      println("Automatically starting wallets ...")
      startWallets(m, bot, lang)
    }

    // filter general buttons
    incomingMsg match {
      case "/start" => startWallets(m, bot, lang)

      case `faucetCmd` => {
        val keyboard = get_faucet_coin_menu_keyboard(lang)
        if (keyboard.getKeyboard.size() < 2) {
          save_state(m, FAUCETMENU)
        } else
          throw new CatchError(Errata.EMPTYFAUCET)
      }

      case `cancelCmd` => get_state(m.getFrom.getId) match {
        case SPEND | SPEND_COIN | SPEND_COIN_TO | SPEND_COIN_TO_AMOUNT => {
          save_state(m, MAINMENU)
          save_db(USER_META_COLLECTION, m.getFrom.getId, "data", document())
        }
        case LOCK | LOCK_COIN | LOCK_COIN_SCRIPT_TO | LOCK_COIN_SCRIPT_TO_AMOUNT | LOCK_COIN_SCRIPT => {
          save_state(m, MAINMENU)
        }
        case a => {
          println(s"TODO CANCEL CASE: ${a}")
          save_state(m, MAINMENU)
          save_db(USER_META_COLLECTION, m.getFrom.getId, "data", document())
        }
      }
      case `backCmd`=> get_state(m.getFrom.getId) match {
        case SPEND_COIN => save_state(m, SPEND)
        case SPEND_COIN_TO => save_state(m, SPEND_COIN)
        case SPEND_COIN_TO_AMOUNT => save_state(m, SPEND_COIN_TO)
        case ADDRESS_LIST => save_state(m, WALLETMENU)
        //case LOCK | LOCK_COIN | LOCK_COIN_IN_CONTRACT => save_state(m, MAINMENU)
        case a => {
          println(s"TODO BACK CASE: ${a}")
          save_state(m, MAINMENU)
        }
      }
      case x => {} //println(s"Unmatched handler: ${x}")
    }

    val state = get_state(m.getFrom.getId())

    println(s"Current state: ${state}")

    val sendRequest : SendMessage = state match {
      case STARTSTATE | MAINMENU => {
        messageOnMainMenu(m, lang)
      }
      case SWAPSMENU => {
        messageOnSwapsMenu(m, lang)
      }
      case FAUCETMENU => {
        messageOnFaucetMenu(m, lang)
      }
      case WALLETMENU | WALLET_PEERS => {
        messageOnWallets(m, lang)
      }
      case ADDRESS_LIST => {
        messageOnAddressList(m, lang)
      }
      case SETTINGSMENU => {
        messageOnSettings(m, lang)
      }
      case LANGUAGEMENU => {
        messageOnLanguage(m, lang)
      }
      case SPEND | SPEND_COIN |
           SPEND_COIN_TO | SPEND_COIN_TO_AMOUNT | SPEND_COIN_TO_NTX => {
        messageOnSpend(m, lang)
      }
      case LOCK | LOCK_COIN | LOCK_COIN_SCRIPT | LOCK_COIN_SCRIPT_TO | LOCK_COIN_SCRIPT_TO_AMOUNT => {
        messageOnLock(m, lang)
      }
      case ADD_NEW_PAYEE | ADD_NEW_PAYEE_ADDRESS => {
        messageOnNewPayee(m, lang)
      }
      case BROADCAST_PAYEE_NTX => {
        val (payee, isbip47, coin, kit) =
          try {
            validate_payee(m, get_meta_string(m, "payee_address"))
          } catch {
            case e@CatchError(Errata.NEEDS_BIP47_CHANNEL) => {
              val (coin, kit) = validate_coin(m, get_meta_string(m, "coin"))
              val payee_address = get_meta_string(m, "payee_address")
              (payee_address.get, true, coin, kit)
            }
          }
        val ntxhex = get_meta_string(m, "ntxhex")
        val ntxhash = get_meta_string(m, "ntxhash")

        val encodeCmd = getEncodeCommand(lang)
        val broadcastCmd = getBroadcastCommand(lang)

        m.getText match{
          case `encodeCmd` => replyTextMessage(m, ntxhex.get)
          case `broadcastCmd` => {
            print(s"Broadcasting notification tx to payee" +
              s"${payee}")

            val tx = new Transaction(kit.getParams, Utils.HEX.decode(ntxhex.get))
            assert(tx!=null)
            assert(tx.getHashAsString.equals(ntxhash.get))
            println(s"Broadcasting ntx: ${tx.getHash}")
            kit.broadcastTransaction(tx)

            kit.putPaymenCodeStatusSent(payee, tx)
            kit.saveBip47MetaData()

            clean_meta(m)
            save_state(m, MAINMENU)
            sendChooseOptionMessage(m,get_main_menu_keyboard(lang),lang)
          }
        }
      }
      case _ => {
        println(s"Unhandled state: ${state}")
        messageOnMainMenu(m, lang)
      }
    }

    if (sendRequest.getText != null)
      bot.execute[Message, BotApiMethod[Message]](sendRequest)
    else
      println("WARN, no text!")
  }

  import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard
  import org.telegram.telegrambots.meta.api.methods.send.SendMessage

  def sendMessageDefault(m: Message, lang: String): SendMessage = {
    val markup = get_main_menu_keyboard(lang)
    save_state(m, MAINMENU)
    return sendHelpMessage(m.getChatId, m.getMessageId, markup, lang)
  }

  /*
   * Wallets
   *   1 BSV
   *   0.15 BCH
   *   0.0157 BTC
   *
   *   To pay (Locked):
   *      0.0157 BTC (05/15)
   *   To collect (Locked):
   *      1 BSV (05/15)
   *
   *   * Transactions
   *   * Addresses
   *   * BCH
   *   * BSV
   */

  def sendChooseOptionMessage(m: Message,
                              replyKeyboard: ReplyKeyboard, lang: String,
                              text: String = "Choose option"
                             ): SendMessage = {
    //println(s"sendChooseOptionMessage: ${text}")
    val sendMessage: SendMessage = new SendMessage()
    sendMessage.enableMarkdown (true)
    sendMessage.setChatId (m.getChatId.toString)
    sendMessage.setReplyToMessageId (m.getMessageId)
    sendMessage.setReplyMarkup (replyKeyboard)
    sendMessage.setText (text)
    return sendMessage
  }

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

  def replyError(m: Message, e: Errata.Value, lang: String): SendMessage= {
    println(s"Replying with error: ${e.toString}")
    replyTextMessage(m, e.toString())
  }

  def sendHelpMessage(chatId: Long, messageId: Integer, markup: ReplyKeyboardMarkup, lang: String): SendMessage = {
    val sendMessage = new SendMessage()
    sendMessage.enableMarkdown(true)
    sendMessage.setChatId(chatId)
    sendMessage.setReplyToMessageId(messageId)
    sendMessage.setReplyMarkup(markup)
    sendMessage.setText(getHelpMessage(lang))
    return sendMessage
  }

  def nextAddress(kit: BIP47AppKit, channel: BIP47Channel): Address = {
    assert(channel!=null)
    val payee_address = kit.getCurrentOutgoingAddress(channel)
    println(s"Incrementing outgoing: ${channel.getCurrentOutgoingIndex}")
    channel.incrementOutgoingIndex()
    println(s"Adding ${payee_address} to outgoing")
    channel.addAddressToOutgoingAddresses(payee_address)
    kit.saveBip47MetaData()
    Address.fromString(kit.getParams, payee_address)
  }

  // validation functions
  def validate_coin (m: Message, s: Option[String]): (String, BIP47AppKit) =
    s.fold(throw new CatchError(Errata.MISSING_META_COIN))(coin => {

      println(s"Validating coin: ${coin}")

      if (false == wallMap.contains(m.getFrom.getId))
          throw new CatchError(Errata.NEEDS_START)

      if (false == wallMap(m.getFrom.getId).contains(coin)) {
        println(s"Invalid coin: ${coin}")
        throw new CatchError(Errata.INVALID_COIN)
      } else {
        save_meta(m, "coin", BSONString(coin))
        (coin, wallMap(m.getFrom.getId)(coin))
      }
    })

  // validates payee and coin
  def validate_payee(m: Message,
                     s: Option[String],
                     replaceBip47Addr: Boolean = true
                    ): (String, Boolean, String, BIP47AppKit) =
    s.fold(throw new CatchError(Errata.MISSING_META_PAYEE))(address_text => {
      println(s"Validating payee: ${address_text}")
      val (coin,kit) = validate_coin(m, get_meta_string(m, "coin"))
      var isBip47Payment = false
      var payee_address = address_text

      if (false == kit.isValidAddress(payee_address)) {
        // is contact?
        val payee = payees(m).find(_._1 == payee_address)
        if (payee.isDefined) {
          payee_address = payee.get._2
        }
      }
      try {
        var paycode = new BIP47PaymentCode(payee_address)
        isBip47Payment = paycode.isValid
      } catch {
        case e:Throwable => {}
      }

      println(s"Is bip47 payment? ${isBip47Payment}")

      if (false == isBip47Payment) {
        val meta = kit.getBip47MetaForOutgoingAddress(payee_address)
        println(s"False... then meta? ${meta}")
      }

      val address : String =
        if (isBip47Payment) {
          println("Bip47 payment detected")
          val channel : BIP47Channel = kit.getBip47MetaForPaymentCode(payee_address)

          println(s"Got channel: ${channel}")

          if (channel == null || false == channel.isNotificationTransactionSent) {
            save_meta(m, "payee_address", BSONString(payee_address))
            throw new CatchError(Errata.NEEDS_BIP47_CHANNEL)
          } else {
            println("Channel is ready")

            if (replaceBip47Addr) {
              val address = nextAddress(kit, channel)
              payee_address = address.toString

              val wallet = kit.getvWallet()
              val addressOfReceived = Some(payee_address)
              println(s"Looking for bip47 address in ${wallet.getImportedKeys.size()} imported ")
              val bip47Address: Option[Address] = wallet
                .getImportedKeys.map(k => LegacyAddress.fromKey(kit.getParams, k))
                .find(adr => formatAddress(adr, coin).equals(formatAddress(address, coin)))

              println(s"Bip47 address: ${bip47Address}")
              println(s"Channel from address: ${kit.getBip47MetaForOutgoingAddress(payee_address)}")
            }

            payee_address
        }
      } else {
        payee_address
      }

      save_meta(m, "payee_address", BSONString(address))
      (address, isBip47Payment, coin, kit)
    })

  def validate_send_amount(m: Message,
                           s: Option[String],
                           r: Boolean = true
                          ): (Coin, String, Boolean, String, BIP47AppKit) =
    s.fold(throw new CatchError(Errata.MISSING_META_AMOUNT))(amount_text => {

      println(s"Validating send amount: ${amount_text} ...")
      val (payee_address, isbip47, coin, kit) =
        validate_payee(m, get_meta_string(m, "payee_address"), r)

      val amount = amount_text.replace("BTC","").replace(coin, "")
      println(s"Validating send amount: ${amount}")

      var spendAmount = Coin.ZERO

      try {
        if (amount.contains("%")) {
          val pct = Integer.parseInt(amount.replace("%",""))
          if (pct == 100) {
            spendAmount = kit.getBalance
            var missing = true
            while(missing) {
              try {
                val overPaid = kit.createSend(payee_address, spendAmount.value)
                missing = false
              } catch {
                case e: InsufficientMoneyException => {
                  spendAmount = spendAmount.minus(e.missing)

                  if (spendAmount.isNegative)
                    throw new CatchError(Errata.INSUFFICIENT_BALANCE)
                  println("Should work")
                }
              }
            }
          } else
            spendAmount = spendAmount.add(Coin.valueOf((pct*kit.getBalance().value/100).toLong))
        } else {
          val n = (amount.toDouble*Coin.COIN.value).toLong
          spendAmount = Coin.valueOf(n)
        }
      } catch {
        case e :Exception => {
          println(s"${e.toString}. Failed to process message: " + e.getStackTraceString)
          throw CatchError(Errata.INVALID_AMOUNT)
        }
      }

      if (spendAmount.isGreaterThan(kit.getBalance)) {
        throw new CatchError(Errata.INVALID_AMOUNT)
      }

      save_meta(m, "amount", BSONLong(spendAmount.value))
      (spendAmount, payee_address, isbip47, coin, kit)
    })

    // Blockhain menu => next_state
    def sendChooseCoinMessage(m: Message, next_state: StateName.Value,
                            hint_text: String,
                            replyKeyboard: ReplyKeyboard, lang: String): SendMessage = {

      val (coin, kit) = validate_coin(m, Some(m.getText))

      var text = s"Balance: ${formatBalance(kit)}\n"
      text += hint_text

      save_meta(m, "coin", BSONString(coin))
      save_state(m, next_state)

      sendChooseOptionMessage(m, replyKeyboard, lang, text)
  }

  def messageOnSpend(m: Message, lang: String) : SendMessage = if (m.hasText) {
    //println(s"messageOnSpend: ${m.getText}")

    if (false == wallMap.contains(m.getFrom.getId)) {
      print("Error: user does not have wallets")
      return replyTextMessage(m, "Error: You don't have wallets. Click: /start")
    }

    val state = get_state(m.getFrom.getId)

    val wallets = wallMap(m.getFrom.getId)

    state match {
      case SPEND => {
        val (coin, kit) = validate_coin(m, Some(m.getText))
        val keyboard = get_choose_payee_menu_keyboard(m, lang)
        val text = getEnterAddressAsk(lang)
        save_state(m, SPEND_COIN)
        sendChooseOptionMessage(m, keyboard, lang, text)
      }

      // SPEND COIN_TO :: Payee address => SPEND_COIN_TO => Enter amount
      case SPEND_COIN => {

        if (m.getText.equals(getAddPayeeCommand(lang))) {
          save_meta(m, "next_state", BSONString(SPEND_COIN.toString))
          save_state(m, ADD_NEW_PAYEE)
          return sendChooseOptionMessage(m, get_cancel_menu_keyboard(lang), lang, getAskNewPayeeAddress(lang))
        }

        val (payee_address, is_bip47, coin, kit) =
          validate_payee(m, Some(m.getText))

        save_state(m, SPEND_COIN_TO)

        val t = s"Balance: ${formatBalance(kit)}\n" + getSpendAmountAsk(lang)
        val k = get_spend_amount_menu_keyboard(lang)
        sendChooseOptionMessage(m, k, lang, t)
      }

      // SPEND_COIN_TO :: Enter amount => SPEND_COIN_TO_AMOUNT
      case SPEND_COIN_TO => {

        val (spendAmount, payee_address, isBip47, coin, kit) =
          validate_send_amount(m, Some(m.getText))

        save_state(m, SPEND_COIN_TO_AMOUNT)
        val wall = kit.getvWallet()
        var tx: Transaction = null

        try {
          val addr = Address.fromString(
            kit.getParams, payee_address)
          tx = kit.createSend(addr, spendAmount.value)
        } catch {
          case e: InsufficientMoneyException => {
            return replyError(m, Errata.INSUFFICIENT_BALANCE, lang)
          }
        }

        val total = tx.getValueSentFromMe(wall)
        var text = ""

        text += s"\n${translations((Texts.BROADCAST_TX_CONFIRM.toString, lang))}"
        text += s"\n**${formatAmount(spendAmount, coin)}** to ${payee_address}" +
          s"\n------------------------" +
          s"\n${formatAmount(tx.getFee, coin)} as transaction fee" +
          s"\n------------------------" +
          s"\nTotal ${formatAmount(total, coin)}"

        save_meta(m, "txhex", BSONString(Utils.HEX.encode(tx.bitcoinSerialize())))
        save_meta(m, "txhash", BSONString(tx.getHashAsString))

        return sendChooseOptionMessage(m,
          get_broadcast_tx_menu_keyboard(lang),
          lang, text)
      }

      // SPEND_COIN_TO_AMOUNT :: Broadcast => MAINMENU
      case SPEND_COIN_TO_AMOUNT => {

        val (spendAmount, payee_address, isBip47, coin, kit) =
          validate_send_amount(m,
            get_meta_long(m, "amount")
              .map(v => Coin.valueOf(v).toFriendlyString))

        val txhash = get_meta_string(m, "txhash")
        val txhex = get_meta_string(m, "txhex")

        val wallet = wallMap(m.getFrom.getId)(coin)
        if (false == wallet.isValidAddress(payee_address))
          throw new CatchError(Errata.INVALID_RECIPIENT)

        var addr: Address = Address.fromString(wallet.getParams, payee_address)
        val wall = wallet.getvWallet()

        val encodeCmd = getEncodeCommand(lang)
        val broadcastCmd = getBroadcastCommand(lang)

        m.getText match{
          case `encodeCmd` => replyTextMessage(m, txhex.get)
          case `broadcastCmd` => {
            print(s"Broadcasting send to ${payee_address} of ${formatAmount(spendAmount, coin)} ")

            //val tx : Transaction = wallet.createSend(addr,spendAmount.value)
            val tx = new Transaction(wallet.getParams, Utils.HEX.decode(txhex.get))
            assert(tx!=null)

            println("Setting memo ... ")
            tx.setMemo("1zzz")

            println(s"Broadcasting tx: ${tx.getHash}")
            wallet.broadcastTransaction(tx)

            save_state(m, MAINMENU)
            sendChooseOptionMessage(m,get_main_menu_keyboard(lang),lang)
          }
        }
      }
    }

  } else {
    println("WARNING: Doesn't have text")
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  def createHTLC(hash: String, swapTime: Long, cancelTime: Long): (
      Script, Script, Address, Address, BIP47AppKit
    ) = {
    (null,null,null,null,null)
  }

  def messageOnLock(m: Message, lang: String) : SendMessage = if (m.hasText) {
    println(s"messageOnLock: ${m.getText}")

    if (false == wallMap.contains(m.getFrom.getId)) {
      print("Error: user does not have wallets")
      return replyTextMessage(m, "Error: You don't have wallets. Click: /start")
    }

    val state = get_state(m.getFrom.getId)

    // Alice:

    // 0. LOCK_COIN => Lock menu => LOCK_6HSWAP | Back
    // 1. LOCK_6H_XCSWAP => Coin menu => LOCK_6H_XCSWAP_COIN
    // 2. LOCK_6H_XCSWAP_COIN => Coin menu => LOCK_6H_XCSWAP_RECIPIENT

    state match {
      // 0. LOCK => Coin menu => LOCK_COIN => Script menu
      case LOCK => {
        val keyboard = get_lock_scripts_menu_keyboard(lang)
        val hint_text = "Choose a lock script: "
        sendChooseCoinMessage(m, LOCK_COIN, hint_text, keyboard, lang)
      }

      // 1. LOCK_COIN :: Script menu => LOCK_COIN_SCRIPT => Payee menu
      case LOCK_COIN => {
        val lock_script = m.getText
        val swapCmd = getLockSwapCommand(lang)
        if (false == swapCmd.equals(lock_script))
          return replyTextMessage(m, "Choose a valid script contract")

        save_meta(m, "lock_script", BSONString(lock_script))
        save_state(m, LOCK_COIN_SCRIPT)
        sendChooseOptionMessage(m, get_choose_payee_menu_keyboard(m, lang), lang)
      }

      // 2. LOCK_COIN_SCRIPT :: Payee menu => LOCK_COIN_SCRIPT_TO => Choose amount
      // ask amount, then build p2sh
      case LOCK_COIN_SCRIPT => {

        if (m.getText.equals(getAddPayeeCommand(lang))) {
          save_meta(m, "next_state", BSONString(LOCK_COIN_SCRIPT.toString))
          save_state(m, ADD_NEW_PAYEE)
          return sendChooseOptionMessage(m, get_cancel_menu_keyboard(lang), lang, getAskNewPayeeAddress(lang))
        }

        val (payee_address, isbip47, coin, kit) = validate_payee(m, Some(m.getText), false)
        println(s"Validating payment code: ${payee_address}")
        assert(isbip47)
        val paycode = new BIP47PaymentCode(payee_address)
        save_meta(m, "payee_address", BSONString(payee_address))

        save_state(m, LOCK_COIN_SCRIPT_TO)

        val keyboard = get_spend_amount_menu_keyboard(lang)
        val hint_text = s"Balance: ${formatBalance(kit)}\n" + getLockAmountAsk(lang)
        sendChooseOptionMessage(m, keyboard, lang, hint_text)
      }

      // 2. LOCK_COIN_SCRIPT_TO :: Choose amount => Payee menu => LOCK_COIN_SCRIPT_TO_AMOUNT
      case LOCK_COIN_SCRIPT_TO => {

        val (lockAmount, payee_address, isbip47, coin, kit) =
          validate_send_amount(m, Some(m.getText), false)

        save_meta(m, "amount", BSONLong(lockAmount.value))

        // 0. Get the Bip47 channel
        assert(isbip47)
        val payee_paycode = new BIP47PaymentCode(payee_address)
        val channel = kit.getBip47MetaForPaymentCode(payee_address)

        // 1. Get secret hash
        val haveSwap = new Swap(kit.getParams)
        val (htlc_secret, htlc_secret_hash) = haveSwap.getSecret()
        assert(htlc_secret.size == 20)

        val secret_hex = Utils.HEX.encode(htlc_secret)
        save_db(USER_HTLC, m.getFrom.getId, s"${htlc_secret_hash}", BSONDocument(
          "swap_secret" -> BSONString(secret_hex)
        ))

        //save_meta(m, "htlc_secret", BSONString(Utils.HEX.encode(htlc_secret)))
        save_meta(m, "htlc_secret_hash", BSONString(Utils.HEX.encode(htlc_secret_hash.getBytes)))

        // 2. Get the address used to post the hash to the payee
        val payee_otc_address = kit.getCurrentOutgoingAddress(channel)
        println(s"Payee otc address: ${payee_otc_address}")
        // TODO: do increment after broadcast
        channel.incrementOutgoingIndex()
        channel.addAddressToOutgoingAddresses(payee_otc_address)
        kit.saveBip47MetaData()

        // 3. Transact limit swap
        // Let the payee know, we want to swap
        val want_coin = "tBTC"
        val price: Float = 0.5f
        val locktime : Long = SwapUtils.REFUND_TIME_NEEDED;
        val limitPayload = haveSwap
          .getLimitSwapPayload(want_coin, price, locktime, htlc_secret_hash.getBytes)
        val limitAddress = Address.fromString(kit.getParams, payee_otc_address)
        val limitTxCost = kit.getParams.getMinNonDustOutput
        val limitSwap = SendRequest.to(limitAddress, limitTxCost)

        println(s"Limit payload size: ${limitPayload.size}")
        limitSwap.memo = "swap_req"
        limitSwap.tx.addOutput(Coin.valueOf(0),
          new ScriptBuilder().op(OP_RETURN).data(limitPayload).build())

        //swap_ntx.addOutput(Coin.ZERO, ScriptBuilder.createOpReturnScript(payload))

        kit.getvWallet().completeTx(limitSwap)
        limitSwap.tx.verify()

        save_db(USER_HTLC, m.getFrom.getId,
          s"${htlc_secret_hash}.swap_payee", BSONString(channel.getPaymentCode))

        val swap_ntx_hex = Utils.HEX.encode(limitSwap.tx.bitcoinSerialize())
        save_meta(m, "swap_ntx_hash", BSONString(limitSwap.tx.getHashAsString))
        save_meta(m, "swap_ntx_hex", BSONString(swap_ntx_hex))

        println("Broadcasting ntx ...")
        //val swap_ntx = new Transaction(kit.getParams, Utils.HEX.decode(swap_ntx_hex))
        kit.broadcastTransaction(limitSwap.tx)
        println(s"done: ${limitSwap.tx.getHashAsString}")

        // Make the HTLC
        val htlc_payee_pubkey = payee_paycode.derivePubKeyAt(kit.getParams, 0)
        val htlc_refund_pubkey = kit.getAccount(0).keyAt(0).getPubKey;
          //new BIP47PaymentCode(kit.getPaymentCode).getPubKey
        val htlc_script = haveSwap.getRedeemScript(
          htlc_secret_hash, htlc_refund_pubkey, htlc_payee_pubkey,
          SwapUtils.REFUND_TIME_NEEDED)

        val validP2SH = ScriptPattern.isPayToScriptHash(htlc_script)

        // Save the payee's p2sh address
        // TODO: Whenever the payee refunds, we refund.
        val p2shAddress : Address =
          ScriptBuilder.createP2SHOutputScript(htlc_script)
            .getToAddress(kit.getParams)

        kit.getvWallet().addWatchedScripts(List(htlc_script))

        save_meta(m, "lock_script", BSONString(htlc_script.toString))
        save_meta(m, "lock_script_address", BSONString(p2shAddress.toString))
        save_meta(m, "lock_script_want", BSONString(Base58.encode(want_coin.getBytes())))

        // send hash as OP_RETURN
        val htlc_tx = new Transaction(kit.getParams)
        htlc_tx.addOutput(lockAmount, htlc_script)

        val htlc_tx_req = SendRequest.forTx(htlc_tx)
        kit.getvWallet().completeTx(htlc_tx_req)

        val tx = htlc_tx_req.tx
        val htlc_tx_hex = Utils.HEX.encode(tx.bitcoinSerialize())

        save_meta(m, "htlc_tx_hash", BSONString(tx.getHashAsString))
        save_meta(m, "htlc_tx_hex", BSONString(htlc_tx_hex))

        // Watch back-swap
        watchLimitSwap(kit, channel, htlc_secret_hash)

        val wall = kit.getvWallet()
        val total = tx.getValueSentFromMe(wall).add(tx.getFee).minus(tx.getValueSentToMe(wall))
        var text = ""
        text += s"\nDo you want to broadcast the contract?" +
          s"\n------------------------" +
          s"\n${htlc_script.toString}" +
          s"\n------------------------" +
          s"\n**${formatAmount(lockAmount, coin)}** to ${p2shAddress}" +
          s"\n------------------------" +
          s"\n${formatAmount(tx.getFee, coin)} as transaction fee" +
          s"\n------------------------" +
          s"\nTotal ${formatAmount(total, coin)}"

        val keyboard = get_broadcast_contract_menu_keyboard(lang)
        save_state(m, LOCK_COIN_SCRIPT_TO_AMOUNT)
        sendChooseOptionMessage(m, keyboard, lang, text)
      }

      // 2. SPEND_COIN_IN_CONTRACT => Contracts menu => Contract type
      case LOCK_COIN_SCRIPT_TO_AMOUNT => {

        val (spendAmount, payee_address, isBip47, coin, kit) =
          validate_send_amount(m,
            get_meta_long(m, "amount")
              .map(v => Coin.valueOf(v).toFriendlyString), false)

        val swap_ntx_hex = get_meta_string(m, "swap_ntx_hex")
        val script_address = get_meta_string(m, "lock_script_address")
        val scriptbytes = get_meta_string(m, "lock_script")
        val txhash = get_meta_string(m, "htlc_tx_hash")
        val txhex = get_meta_string(m, "htlc_tx_hex")

        val wallet = wallMap(m.getFrom.getId)(coin)
        if (false == wallet.isValidAddress(payee_address))
          throw new CatchError(Errata.INVALID_RECIPIENT)

        var p2shAddress: Address = Address.fromString(wallet.getParams, script_address.get)
        val wall = wallet.getvWallet()

        val scriptCmd = getViewScriptCommand(lang)
        val encodeCmd = getEncodeCommand(lang)
        val broadcastCmd = getBroadcastCommand(lang)

        m.getText match{
          case `scriptCmd` => replyTextMessage(m, scriptbytes.get)
          case `encodeCmd` => replyTextMessage(m, txhex.get)
          case `broadcastCmd` => {
            println(s"Broadcasting contract to ${p2shAddress} of ${formatAmount(spendAmount, coin)} ")
            val tx = new Transaction(wallet.getParams, Utils.HEX.decode(txhex.get))
            assert(tx!=null)

            println(s"Broadcasting tx: ${tx.getHash}")
            wallet.broadcastTransaction(tx)

            save_state(m, MAINMENU)
            sendChooseOptionMessage(m,get_main_menu_keyboard(lang),lang)
          }
        }
      }

        /*
        Anton Tovstonozhenko anton@ambisafe.co a través de googlegroups.com

            I'm using lib to create transaction spending btc from multisig.

            redeemScript = ScriptBuilder.createMultiSigOutputScript(threshold, ecKeys);
            multisigAddress = ScriptBuilder.createP2SHOutputScript(redeemScript).getToAddress(MainNetParams.get()).toString();
            inputScript = ScriptBuilder.createP2SHMultiSigInputScript(null, redeemScript);

            new TransactionInput(tx.getParams(), tx, inputScript.getProgram(), new TransactionOutPoint(tx.getParams(),
                                utxo .getOutputIndex(), new Sha256Hash(utxo.getTransactionHash())), utxo.getAmount());

            tx - transaction where you need to add the input
            utxo - stores info about unspent output:
                private String transactionHash;
                private int outputIndex;
                private Coin amount;
         */
      case LOCK_CONFRMATION => {
        // expect recipient
        val contract = get_meta_string(m, "contract")
        val recipient = m.getText

        // if broadcast
        save_meta(m, "address", BSONString(recipient))
        save_state(m, MAINMENU)

        // ask contract
        replyTextMessage(m, "TODO ASK CONTRACT")
      }
    }
  } else {
    println("WARNING: Doesn't have text")
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  def messageOnNewPayee(m: Message, lang: String) : SendMessage = if (m.hasText) {
    // expect next_state
    val next_state = StateName.withName(get_meta_string(m, "next_state").get)

    val state = get_state(m.getFrom.getId)
    state match {
      case ADD_NEW_PAYEE => {
        // validate payment address
        val address = m.getText
        val firstWallet = wallMap.get(m.getFrom.getId).getOrElse(mutable.Map()).head._2

        if (firstWallet.isValidAddress(address)) {
          val text = getAskNewPayeeName(lang)
          save_meta(m, "payee_address", BSONString(address))
          save_state(m, ADD_NEW_PAYEE_ADDRESS)
          sendChooseOptionMessage(m, get_back_menu_keyboard(lang), lang, text)
        } else
          replyError(m, Errata.INVALID_RECIPIENT, lang)
          //replyTextMessage(m, address)
      }
      case ADD_NEW_PAYEE_ADDRESS => {
        val (payee_address, isbip47, coin, kit) =
          validate_payee(m, get_meta_string(m.getFrom.getId, "payee_address"))
        //get_meta_string(m, "payee_address").get
        val payee_name = m.getText
        val data : BSONDocument =
          document("payee_name" -> BSONString(payee_name),
            "payee_address" -> BSONString(payee_address))

        insert_db(USER_PAYEE_COLLECTION, m.getFrom.getId, "data", data)
        replyTextMessage(m, "Payee added. ")

        save_meta(m, "payee_address", BSONString(payee_address))
        save_state(m, next_state)
        if (isbip47) {
          kit.saveBip47MetaData()
          kit.loadBip47MetaData()
        }
        next_state match {
          case SPEND_COIN => {
            messageOnSpend(m, lang)
          }
          case LOCK_COIN_SCRIPT => {
            messageOnLock(m, lang)
          }
        }
      }
    }
  } else {
    println("WARNING: Doesn't have text")
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  def viewWallets(m:Message, lang:String):SendMessage = {
    save_state(m, WALLETMENU)
    val wallets : mutable.Map[String, BIP47AppKit] = wallMap.getOrElse(m.getFrom.getId, mutable.Map())
    var txt : String = "Wallets:\n"

    // Payment address
    val pcodes : mutable.Set[String] = mutable.Set()
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

  def viewSwaps(m:Message, lang:String):SendMessage = {
    save_state(m, SWAPSMENU)

    val wallets = wallMap.getOrElse(m.getFrom.getId, mutable.Map())
    var txt : String = "Locked:\n"
    val keyboard = get_swaps_menu_keyboard(lang)
    println("Replying on view swaps keyboard")
    sendChooseOptionMessage(m, keyboard, lang, txt)
  }

  def listHTLCS(wallets: List[BIP47AppKit]): List[Transaction] = {
    assert(wallets.size > 0)
    val isHTLC : Transaction => Boolean = (tx: Transaction) => {
      tx.getOutputs.exists (
        out => SwapUtils.maybeRedeemableSwap (out.getScriptPubKey).isDefined)
    }
    wallets.map(_.getTransactions).flatMap(txList => txList.filter(isHTLC(_)))
  }

  def showHTLC(htlc: Transaction, wallet: BIP47AppKit, i: Int, n: Int, lang: String): String = {
    val output = htlc.getOutputs.find( out =>
      SwapUtils.maybeRedeemableSwap(out.getScriptPubKey).isDefined)

    val script = output.get.getScriptPubKey
    val isSpendable = output.get.isAvailableForSpending
    val isMine = output.get.isMine(wallet.getvWallet())

    val redeemableSwap = SwapUtils.maybeRedeemableSwap(script)
    val (hash,whoFunded,whoSwaps,locktime) = redeemableSwap.get

    val fundKey = ECKey.fromPublicOnly(whoFunded)
    val payeeKey = ECKey.fromPublicOnly(whoSwaps)
    val fundedByOwner = wallet.getvWallet().hasKey(fundKey)
    val fundedByPayee = wallet.getvWallet().hasKey(payeeKey)

    var txt = s"HTLC (${i+1} of ${n})."
    txt += s"\n${translations(Texts.TX_HASH.toString, lang)}: ${htlc.getHashAsString}"

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

  def messageOnMainMenu(m: Message, lang: String): SendMessage = if (m.hasText) {
    println(s"messageOnMainMenu: ${m.getText}")
    val walletsCmd = getWalletsCommand(lang)
    val swapsCmd = getHTLCListCommand(lang)
    val spendCmd = getSpendCommand(lang)
    val lockCmd = getLockCommand(lang)
    val settingsCmd = getSettingsCommand(lang)

    m.getText() match {

      case `walletsCmd` => viewWallets(m, lang)

      case `swapsCmd` => viewSwaps(m, lang)

      case `spendCmd` => {
        save_state(m, SPEND)

        val keyboard = get_spend_coin_menu_keyboard(m.getFrom.getId, lang)
        if (spendable_coins(m.getFrom.getId).size > 0)
          sendChooseOptionMessage(m, keyboard, lang)
        else throw new CatchError(Errata.MISSING_SPEND_COINS)
      }

      case `lockCmd` => {
        save_state(m, LOCK)
        sendChooseOptionMessage(m, get_spend_coin_menu_keyboard(m.getFrom.getId, lang), lang)
      }

      case `settingsCmd` => {
        save_state(m, SETTINGSMENU)
        sendChooseOptionMessage(m, get_settings_menu_keyboard(m.getFrom.getId, lang), lang)
      }

      case _ => sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
    }
  } else {
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  def messageOnSwapsMenu(m: Message, lang: String): SendMessage = {
    assert(m.hasText)
    val lockSwapCmd = getLockSwapCommand(lang)
    val htlcListCmd = getHTLCListCommand(lang)

    m.getText match {
      case `lockSwapCmd` => {
        save_state(m, LOCK)
        sendChooseOptionMessage(m, get_spend_coin_menu_keyboard(m.getFrom.getId, lang), lang)
      }
      case `htlcListCmd` => {

        val wallets = wallMap.getOrElse(m.getFrom.getId, mutable.Map()).values.toList
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
            getTransactionLink(coin, first), lang,0, htlcList.size)
          reply.setReplyMarkup(inlineKeyboard)
          reply.setText(showHTLC(htlcList.head, wallet.get, 0, htlcList.size, lang))
        }

        reply
      }
    }
  }

  def messageOnFaucetMenu(m: Message, lang: String): SendMessage = if (m.hasText) {

    if (m.getText.equals(getFaucetCommand(lang)))
      return sendChooseOptionMessage(m, get_faucet_coin_menu_keyboard(lang), lang)

    // validate input
    val (coin, userKit) = validate_coin(m, Some(m.getText))

    // user has meta coin
    // get admin's wallet
    val adminKit = wallMap(adminId)(coin)

    // where to send faucet?
    val adr = userKit.getCurrentAddress
    val tx = adminKit.createSend(adr, faucetCoin.value)
    adminKit.broadcastTransaction(tx)

    // should move to SPEND_COIN
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  } else {
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  def messageOnAddressList(m: Message, lang: String): SendMessage = if (m.hasText) {
    val state = get_state(m.getFrom.getId)
    val (coin, kit) = validate_coin(m, Some(m.getText))
    var reply = "Addreses:\n"

    reply = reply + kit.getCoinName + "\n"
    val wallet = kit.getvWallet()

    val issued: Seq[String] = wallet.getIssuedReceiveAddresses.map(adr => { formatAddress(adr, coin) })

    val ps = wallet.getParams
    val imported: Seq[String] =
      wallet.getImportedKeys
        .map(adr => LegacyAddress.fromKey(ps, adr))
        .map(adr => formatAddress(adr, coin))

    val watched: Seq[String] =
      wallet.getWatchedAddresses.map(adr => formatAddress(adr, coin))

    val acc = new BIP47Account(kit.getParams, kit.getPaymentCode)

    reply += s"*Notification address:* ${formatAddress(acc.getNotificationAddress, coin)}\n"
    reply += s"*Issued:* \n${issued.mkString("\n\t")}\n"
    reply += s"*Imported:* \n${imported.mkString("\n\t")}\n"
    reply += s"*Watched:* \n${watched.mkString("\n\t")}\n"

    save_state(m, MAINMENU)
    replyTextMessage(m, reply)
  } else {
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  def messageOnWallets(m: Message, lang: String): SendMessage = if (m.hasText) {
    val walletsCmd = getWalletsCommand(lang)
    val txsCmd = getTransactionsCommand(lang)
    val spendCmd = getSpendCommand(lang)
    val lockCmd = getLockCommand(lang)
    val addrCmd = getAddressListCommand(lang)
    val peersCmd = getPeersCommand(lang)

    m.getText match {

      case `walletsCmd` => {
        viewWallets(m, lang)
      }

      case `txsCmd` => {
        var reply= "Transactions:\n"

        val wallets : UserWallets = wallMap.getOrElse(m.getFrom.getId, mutable.Map())
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
            } else {
              if (tx.getConfidence.getSource == TransactionConfidence.Source.SELF) {
                println(s"removing tx: ${tx}")
                elem._2.unsafeRemoveTxHash(tx.getHash)
              } else {
                reply += s"Confidence: ${tx.getConfidence}\n"
              }

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

      case `addrCmd`  => {
        save_state(m, ADDRESS_LIST)
        val keyboard = get_all_coins_menu_keyboard(m.getFrom.getId, lang)
        sendChooseOptionMessage(m, keyboard, lang)
      }

      case `spendCmd` => {
        save_state(m, SPEND)

        val keyboard = get_spend_coin_menu_keyboard(m.getFrom.getId, lang)
        if (spendable_coins(m.getFrom.getId).size > 0)
          sendChooseOptionMessage(m, keyboard, lang)
        else throw new CatchError(Errata.MISSING_SPEND_COINS)
      }

      case `lockCmd` => {
        save_state(m, LOCK)
        sendChooseOptionMessage(m, get_spend_coin_menu_keyboard(m.getFrom.getId, lang), lang)
      }

      case `peersCmd` => {
        var reply = "*Connections:*\n"
        val wallets : UserWallets = wallMap.getOrElse(m.getFrom.getId, mutable.Map())
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
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  def messageOnSettings(m: Message, lang: String): SendMessage = if (m.hasText) {
    val seedsCmd = getSeedCommand(lang)
    val whoamiCmd = getWhoamiCommand(lang)
    val infoCmd = getInfoCommand(lang)
    val langCmd = getLanguageCommand(lang)

    m.getText match {
      case `seedsCmd` => {
        var text = ""

        val wallets = wallMap(m.getFrom.getId)
        val pcs : mutable.Set[String] = mutable.Set()
        wallets.values.foreach(w => pcs.add(w.getMnemonicCode))
        assert(pcs.size==1)
        replyTextMessage(m, pcs.head)
      }
      case `whoamiCmd` => {
        save_state(m, WHOAMIMENU)

        val wallets : mutable.Map[String, BIP47AppKit] = wallMap.getOrElse(m.getFrom.getId, mutable.Map())

        // Payment address
        val pcodes : mutable.Set[String] = mutable.Set()
        for (elem <- wallets) {
          pcodes += elem._2.getPaymentCode
        }
        assert(pcodes.size == 1)
        var txt = s"Universal address: ${pcodes.head}"

        val sendPhotoRequest = new SendPhoto()

        sendPhotoRequest.setChatId(m.getChatId)
        sendPhotoRequest.setPhoto(qrFile(m))
        sendPhotoRequest.setCaption(txt)
        throw new SendPhotoCatch(sendPhotoRequest)
      }
      case `infoCmd` => {
        var text = "info"
        replyTextMessage(m, text)
      }
      case `langCmd` => {
        save_state(m, StateName.LANGUAGEMENU)
        val keyboard = get_language_menu_keyboard(lang)
        sendChooseOptionMessage(m, keyboard, lang)
      }
    }
  } else {
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  def messageOnLanguage(m: Message, lang: String): SendMessage = if (m.hasText) {
    val esLang = getSpanishLanguageCommand(lang)
    val enLang = getEnglishLanguageCommand(lang)
    m.getText match {
      case `esLang` => {
        set_language(m.getFrom.getId, "es")
      }
      case `enLang` => {
        set_language(m.getFrom.getId, "en")
      }
      case _ => throw new CatchError(Errata.INVALID_LANGUAGE)
    }
    val new_lang = get_language(m)
    save_state(m, MAINMENU)
    sendChooseOptionMessage(m, get_main_menu_keyboard(new_lang), new_lang)
  } else {
    sendChooseOptionMessage(m, get_main_menu_keyboard(lang), lang)
  }

  class XCSwaps_Bot(token: String) extends TelegramLongPollingBot {
    override def getBotToken = token

    override def onUpdateReceived(update: Update): Unit = {

      if (update.hasMessage() && update.getMessage.hasText && update.getMessage.getText().equals("ping")) {
        import org.telegram.telegrambots.meta.api.methods.send.SendMessage
        import org.telegram.telegrambots.meta.exceptions.TelegramApiException
        val message : BotApiMethod[Message] = new SendMessage().setChatId(update.getMessage.getChatId).setText(update.getMessage.getText)
        try {
          execute[Message, BotApiMethod[Message]](message) // Call method to send the message
        }
        catch {
          case e: TelegramApiException =>
            e.printStackTrace
        }
      }

      try {
        if (false == update.hasCallbackQuery)
          recv_up(update, this)
        else
          recv_call(update, this)
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }

    override def getBotUsername = "XCSwaps_Bot"

    override def clearWebhook(): Unit = println("CLEARING WEBHOOK!!")
  }

  def main( args:Array[String] ):Unit =  {

    val langs = List("en","es")
    // validate translations
    for (l <- langs) {
      for (t <- Texts.values) {
        val exists = translations.contains((t.toString, l))
        assert(exists, s"Text translation missing: ${t.toString}, ${l}")
      }
    }

    if (args.size < 1) {
      println("ARG 0: <BOT API TOKEN>")
    } else {
      Main.api_token= args(0)
      ApiContextInitializer.init();
      val api = new TelegramBotsApi();

      if (args.size > 1)
        Main.run_env = args(1)
      api.registerBot(new XCSwaps_Bot(Main.api_token))
    }

  }
}
