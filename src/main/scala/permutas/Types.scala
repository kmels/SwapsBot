package permutas

import constants.{Callbacks, Labels}

import org.bitcoinj.core._
import org.bitcoinj.kits._
import org.bitcoinj.script._
import scala.collection.mutable

package object Types {
  // *** Primitive
  type Pubkey = Array[Byte]

  // *** HTLC
  type LimitSwap = (String, Float, Long, Sha256Hash)
  type HTLCFromTx = (NetworkParameters, BIP47AppKit, Transaction, TransactionOutput, Script)
  type RedeemableSwap = (Sha256Hash, Pubkey, Pubkey, Long)

  // *** Keyboard
  type LinkOrText = Either[String, Callbacks.Value]
  type MsgButton = (Labels.Value, LinkOrText)

  // *** Wallets
  type UserWallets = mutable.Map[String, BIP47AppKit]
  type WallMap = mutable.Map[Int, mutable.Map[String, BIP47AppKit]]

}
