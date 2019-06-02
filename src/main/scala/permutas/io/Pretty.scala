package permutas.io

import org.bitcoinj.core._
import org.bitcoinj.kits.BIP47AppKit

package object Pretty {
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

  def formatLink(text: String, url: String): String = s"[${text}](${url})"

  def formatTxLink(coin: String, tx: Transaction): String = formatLink(text = tx.getHashAsString.take(6),url = getTransactionLink(coin,tx))

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
    ).get(coin)
    val defaulturl = s"https://blockchair.com/bitcoin-sv/transaction/TXHASH"
    baseurl.getOrElse(defaulturl)
      .replace("TXHASH", tx.getHashAsString)
  }
}
