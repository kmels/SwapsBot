package permutas

import constants.{Callbacks, Labels}

import org.bitcoinj.core._
import org.bitcoinj.kits._
import org.bitcoinj.script._
import scala.collection.mutable

package object Types {

  type Pubkey = Array[Byte]
  type LimitSwap = (String, Float, Long, Sha256Hash)
  type HTLCFromTx = (NetworkParameters, BIP47AppKit, Transaction, TransactionOutput, Script)
  type KeyboardI18NButton = (Labels.Value, Either[String, Callbacks.Value])
  type RedeemableSwap = (Sha256Hash, Pubkey, Pubkey, Long)
  type UserWallets = mutable.Map[String, BIP47AppKit]
  type WallMap = mutable.Map[Int, mutable.Map[String, BIP47AppKit]]

}
