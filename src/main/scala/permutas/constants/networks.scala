package permutas.constants

import org.bitcoinj.core.NetworkParameters
import org.bitcoinj.params._

object networks {

  val _networks = Map[String, NetworkParameters] (
    "BSV" -> new BSVMainNetParams(),
    "BCH" -> new BCCMainNetParams(),
    "tBCH" -> new BCCTestNet3Params(),
    "BTC" -> new MainNetParams(),
    "tBTC" -> new TestNet3Params()
  )


  def apply(s: String) = _networks(s)
}

