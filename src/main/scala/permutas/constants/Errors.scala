package permutas.constants

object Errors extends Enumeration {
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