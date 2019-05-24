# Atomic Swaps Telegram Bot

This is a bot to exchange bitcoin with one another.
It uses the blockchain and 5 transactions to do it trustless as an escrow agent.


## Run

```
sbt run <BOT API TOKEN>
```

# XCSWAP Protocol

Price is reached by over the counter (OTC) negotiation, consisting of transactions (in order):
  * **Transaction 1.** Seller sends limit swap to buyer. It contains specific secret hash H, market X/Y, Price and Volume.
     * Secret hash H = SHA256(K) is 32 bytes. Secret K is 20 bytes.
     * Market format is X/Y, for example tBTC/BCH. X is underlying (what seller offers), Y is currency.
     * Price is a 4 byte float. Price is the value of a unitary X quoted in Y.
     * Volume is a 4 byte float. Volume is quoted in X.

  * **Transaction 2.** Buyer initiates or fills swap. Buyer locks an Amount of Y, refundable to him after 2 blocks.
  Amount = Volume*Price

An exchange is not complete until after buyer's and seller's goods are transfered.
Some transactions may not happen at all, that's when users choose to refund X or Y back if the trade expired.
To fulfill the trade an atomic swap happens when in order:

  * **Transaction 3.** Seller locks the underlying Amount to Buyer and pays a fee.

  * **Transaction 4.** Seller spends buyer's currency Y, deposits to cashier. Sseller reveals the
secret for spending the underlying X and pays a fee.

  * **Transaction 5.** Buyer sweeps the underlying. Buyer uses the secret to receive the underlying Volume of X and pays a fee.

Transaction 1. Limit transaction. Consists of 48 bytes
----
  * [want coin][price][locktime][secret hash]
  * => [4bytes][4bytes][8 byte int][32bytes] = [48 bytes]

Transaction 2. Buyer fills limit using it's hash lock, locks Amount of Y.
Conditions for spending Amount of Y are (either works):
----
  * Seller reveals secret K (transaction 4).
  * Seller didn't want to trade the price, Buyer refunds after 2 blocks are mined without seller locking underlying X. Note: Seller should not lock Volume of X after this refund.

Transaction 3. Seller locks underlying X .
Conditions for spending and taking the underlying X are (either works):
-----
  * Buyer waits for Transaction 4 with K, unlocks underlying X.
  * Buyer doesn't sweep, Seller can refund after 2 blocks without buyer (tx 5) and/or seller (tx 4) using the secret.

Transaction 4. Seller spends Transaction 2 utxo, takes and receives Amount of Y and reveals secret K
-----

Transaction 5. Buyer spends Transaction 3 utxo, takes and receives underlying X
-----

