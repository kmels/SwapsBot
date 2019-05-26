package permutas

import org.telegram.telegrambots.meta.api.objects.Message
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

package object MongoDb {

  import permutas.constants._
  import Collections._

  import reactivemongo.api.{Cursor, DefaultDB, MongoConnection, MongoDriver}
  import reactivemongo.bson.{BSONDocumentReader, BSONDocumentWriter, Macros, document}
  import reactivemongo.bson.BSONDocument
  import reactivemongo.bson._

  /** ****
    * BEGIN DATABASE LAYER
    * ******/

  val mongoUri = "mongodb://localhost:27018" ///xcswaps_dev"
  import ExecutionContext.Implicits.global

  val driver = MongoDriver()
  val parsedUri = MongoConnection.parseURI(mongoUri)
  val connection = parsedUri.map(driver.connection(_))

  val futureConnection = Future.fromTry(connection)

  def db = futureConnection.flatMap(_.database(s"xcswaps_demo"))

  def col(s: Collections.Value) = db.map(_.collection(s.toString()))

  def get_collection_value(collection: Collections.Value, who: Integer): Option[BSONValue] = {
    get_collection_value(collection, who, None, None)
  }

  def get_collection_value(collection: Collections.Value, who: Integer, key: String): Option[BSONValue] = {
    get_collection_value(collection, who, Some(key), None)
  }

  def get_collection_doc_value(collection: Collections.Value, who: Integer, data_key: String): Option[BSONDocument] = {
    get_collection_value(collection, who, data_key = data_key) match {
      case Some(BSONDocument(e)) => {
        Some(BSONDocument(e))
      }
      case _ => None
    }
  }

  def get_collection_string_value(collection: Collections.Value,
                                  who: Integer, key: String, data_key: String = "data"): Option[String] = {
    get_collection_value(collection, who, Some(key), None, data_key = data_key) match {
      case Some(BSONString(s)) => Some(s)
      case _ => None
    }
  }

  def get_collection_value(collection: Collections.Value,
                           who: Integer,
                           key: String,
                           fallback_value: BSONValue): Option[BSONValue] = {
    get_collection_value(
      collection,
      who,
      if (key.isEmpty) None else Some(key),
      Some(fallback_value))
  }

  def get_collection_value(collection: Collections.Value,
                           who: Integer, doc_key: Option[String] = None,
                           fallback_value: Option[BSONValue] = None,
                           data_key: String = "data"
                          ): Option[BSONValue] = {

    // Accept key "data.key" instead of just "key" and a hardcoded data
    var ret: BSONDocument = null
    val query = BSONDocument("who" -> BSONInteger(who))
    val doc: Future[Option[BSONDocument]] = col(collection).flatMap(_.find(query).one[BSONDocument])

    val it: Option[BSONDocument] = Await.result(doc, 2 seconds)

    val transformed: Option[BSONValue] = it.flatMap(bson => {

      val (dataOpt, key): (Option[BSONValue], Option[String]) =
        if (data_key.contains(".") && false == bson.contains(data_key)) {
          val docOpt = bson.get(data_key.takeWhile(_ != '.'))
          val dockey = Some(data_key.dropWhile(_ != '.').tail)
          (docOpt, dockey)
        }
        else {
          (bson.get(data_key), doc_key)
        }

      if (false == dataOpt.isDefined) {
        println(s"${who} Did not find data_key=${data_key} and key=${key}")
        return None
      }
      val bsondata = dataOpt.get

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

  /** ****
    * END DATABASE LAYER
    * ******/

  def get_language(m: Message): String =

    get_collection_value(LANGUAGE_COLLECTION, m.getFrom.getId, "lang") match {
      case Some(BSONString(value)) => value
      case _ => get_collection_value(LANGUAGE_COLLECTION, m.getChatId.toInt, "lang") match {
        case Some(BSONString(value)) => value
        case _ => "es"
      }
    }


  def set_language(who: Integer, lang: String) = save_db(LANGUAGE_COLLECTION, who, "data", document("lang" -> lang))

  def get_meta_string(who: Integer, key: String): Option[String] =
    get_collection_value(USER_META_COLLECTION, who, key).flatMap(optCoin => optCoin match {
      case BSONString(value) => Some(value)
      case ext => {
        println(s"get_meta_string ${key} Match error: now got: ${ext}")
        None
      }
    }) match {
      case None => {
        println(s"WARNING: Missing meta: ${key}"); None
      }
      case x => x
    }

  def get_meta_string(m: Message, key: String): Option[String] = get_meta_string(m.getFrom.getId, key)

  def get_meta_long(who: Integer, key: String): Option[Long] =
    get_collection_value(USER_META_COLLECTION, who, key).flatMap(optCoin => optCoin match {
      case BSONLong(value) => Some(value)
      case ext => {
        println(s"get_meta_long Match error: now got: ${ext}")
        None
      }
    })

  def get_meta_long(m: Message, key: String): Option[Long] = get_meta_long(m.getFrom.getId, key)

  def get_state(who: Integer): States.Value = {
    //import reactivemongo.bson.Macros
    val st: BSONValue = get_collection_value(USER_STATE_COLLECTION, who, "state", BSONString(States.STARTSTATE.toString())).get
    st match {
      case BSONString(value) => States.withName(value)
      case _ => throw new Exception()
    }

  }

  def save_db(collection: Collections.Value, who: Integer, key: String, data: BSONValue): Unit = {
    val selector = BSONDocument("who" -> BSONInteger(who)) //document("who" -> who)
    val modifier = BSONDocument("$set" -> BSONDocument(key -> data))
    val upsert: Future[reactivemongo.api.commands.UpdateWriteResult] = col(collection)
      .flatMap(_.update(selector, modifier, upsert = true))
    val res = Await.ready(upsert, 2 seconds)
  }

  def insert_db(collection: Collections.Value, who: Integer, key: String, data: BSONValue): Unit = {
    val selector = BSONDocument("who" -> BSONInteger(who)) //document("who" -> who)
    val modifier = BSONDocument("$push" -> BSONDocument(key -> data))
    val upsert: Future[reactivemongo.api.commands.UpdateWriteResult] = col(collection)
      .flatMap(_.update(selector, modifier, upsert = true))
    val res = Await.ready(upsert, 2 seconds)
  }

  def save_meta(m: Message, key: String, value: BSONValue) = {
    println(s"Saving ${key} ... ") //=> ${value}")
    save_db(USER_META_COLLECTION, m.getFrom.getId, s"data.${key}", value)
  }

  def clean_meta(m: Message) =
    save_db(USER_META_COLLECTION, m.getFrom.getId, "data", document())

  def save_state(m: Message, state: States.Value): Unit = {
    val who = m.getFrom.getId
    println(s"Saving state of ${who} to ${state.toString()}")
    val v: BSONDocument = document("state" -> state.toString) //Document("state" -> state.toString()
    save_db(USER_STATE_COLLECTION, who, "data", v)
  }
}