package permutas.wallets

import org.bitcoinj.script.ScriptOpCodes._
import org.bitcoinj.core._
import org.bitcoinj.script.{Script, ScriptBuilder, ScriptChunk, ScriptPattern}
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

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

  def getAddressFromRedeemScript(redeemScript: Script): Address = {
    ScriptBuilder.createP2SHOutputScript(redeemScript).getToAddress(params);
  }

  // 48 bytes:
  // [want coin][price][locktime][secret]
  // [4bytes][4bytes][8 byte int][32bytes] = [48 bytes]
  // want coin ::  String
  // price :: want/have coin, e.g. want 20 tBCH for 1 tBTC (price = 20)
  def getLimitSwapPayload(wantCoin: String, price: Float,
                          locktime: Long, secretHash: Array[Byte]): Array[Byte] = {
    val ret = ArrayBuffer[Byte]()

    assert(wantCoin.size == 3 || wantCoin.size == 4)

    val wantBytes = wantCoin.getBytes
    if (wantCoin.size == 3) {
      ret.insert(0, '-'.toByte)
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
    assert(secret_.size == 20)
    this.secret = Some(secret_)
    return secret_
  }

  def getSecret(): (Array[Byte], Sha256Hash) = {
    if (false == this.secret.isDefined)
      makeSecret()

    (this.secret.get, Sha256Hash.of(this.secret.get))
  }
}
