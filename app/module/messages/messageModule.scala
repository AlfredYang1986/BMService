package module.messages

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

import java.util.Date

object messageModule {
  
	val message_type_text = 0
	val message_type_photo = 1
	val message_type_movie = 2
	
	val receiver_type_tmp_group = 0
	val receiver_type_user = 1
	val receiver_type_user_group = 2

	// Login
	def loginWithName(data : JsValue) : JsValue = {
		
		val user_id= (data \ "user_id").asOpt[String].get
		
		val rel = from db() in "users" where ("user_id" -> user_id) select (x => x)
		if (!rel.empty) {
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("user_id" -> toJson(user_id)))))
		} else {
			val builder = MongoDBObject.newBuilder
			builder += "user_id" -> user_id
			val friend_list = MongoDBList.newBuilder
			builder += "friends" -> friend_list.result
		
			_data_connection.getCollection("users") += builder.result
			/**
			 * return 
			 */
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("user_id" -> toJson(user_id)))))
		}
	}

	// Friends
	def addOneFriend(data : JsValue) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val friend_id = (data \ "friend_id").asOpt[String].get
	
		val user_ref = (from db() in "users" where "user_id" -> user_id select (x => x)).head
		val friend_ref = (from db() in "users" where "user_id" -> friend_id select (x => x)).head
	
		user_ref.getAs[MongoDBList]("friends").map { x => 
		  	if (x.contains(friend_id)) //ErrorCode.errorToJson("already friends")
		  		queryFriends(data)
		  	else {
		  		x += friend_id
		  		_data_connection.getCollection("users").update(DBObject("user_id" -> user_id), user_ref)
		  		
		  		friend_ref.getAs[MongoDBList]("friends").map { y =>
		  		  	y += user_id
		  		  	_data_connection.getCollection("users").update(DBObject("user_id" -> friend_id), friend_ref)
		  		}.getOrElse(throw new Exception)
		  		queryFriends(data)
		  	}
		}.getOrElse(throw new Exception)
	}
	
	def deleteOneFriend(data : JsValue) = {
	
		val user_id = (data \ "user_id").asOpt[String].get
		val friend_id = (data \ "friend_id").asOpt[String].get
	
		val user_ref = (from db() in "users" where "user_id" -> user_id select (x => x)).head
		val friend_ref = (from db() in "users" where "user_id" -> friend_id select (x => x)).head
	
		user_ref.getAs[MongoDBList]("friends").map { x => 
		  	if (x.contains(friend_id)) {
		  		user_ref.update("friends", x.filter(_ != friend_id))
		  		_data_connection.getCollection("users").update(DBObject("user_id" -> user_id), user_ref)
		  		
		  		friend_ref.getAs[MongoDBList]("friends").map { y =>
		  			friend_ref.update("friends", y.filter(_ != user_id))
		  		  	_data_connection.getCollection("users").update(DBObject("user_id" -> friend_id), friend_ref)
		  		}.getOrElse(throw new Exception)
		  		queryFriends(data)
		  	} else queryFriends(data)
		  	
		}.getOrElse(throw new Exception)  
	}
	
	def queryFriends(data : JsValue) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val user_ref = (from db() in "users" where "user_id" -> user_id select (x => x)).head
	
		var re : List[JsValue] = Nil
		user_ref.getAs[MongoDBList]("friends").map { x => 
		 	x.toSeq.map { it => 
		 		re = toJson(it.asInstanceOf[String]) :: re
		 	}
		  
		}.getOrElse(throw new Exception)

		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("friends" -> toJson(re)))))
	}

	// Messages
	// both restful and web socket go through this function
	def sendMessage(data : JsValue) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val date = new Date().getTime.longValue
		val receiver_type = (data \ "receiver_type").asOpt[Int].get
		val receiver_id = (data \ "receiver").asOpt[String].get
		val message_type = (data \ "message_type").asOpt[Int].get
		val message_content = (data \ "message_content").asOpt[String].get
	
		val collection_name = "chat_history"
		
		val builder = MongoDBObject.newBuilder
		builder += "sender" -> user_id
		builder += "receiver_type" -> receiver_type
		builder += "receiver" -> receiver_id
		builder += "date" -> date 
		builder += "message_type" -> message_type
		builder += "message_content" -> message_content
	
		val message = builder.result
		_data_connection.getCollection(collection_name) += message
		/**
		 * return 
		 */
		MessageNotificationModule.defaultNotificationCenter.map { nc =>
			nc ! pushNotification2(message2Json(message)) }
		.getOrElse(
			// TODO: not connect with websocket, so need to use apple notification api
		)
		Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("status" -> toJson("success")))))	
	}
	
	def getField[T](obj : MongoDBObject, name : String) : T = obj.get(name).map(_.asInstanceOf[T]).getOrElse(throw new Exception)
	def getSender(x : MongoDBObject) : String = getField[String](x, "sender")
	def getReceiver(x : MongoDBObject) : String = getField[String](x, "receiver") 
	def getReceiverType(x : MongoDBObject) : Int = getField[Int](x, "receiver_type") 
	def getMessageType(x : MongoDBObject) : Int = getField[Int](x, "message_type")
	def getMessageContent(x : MongoDBObject) : String = getField[String](x, "message_content")
	def getMessageDate(x : MongoDBObject) : Long = getField[Long](x, "date")

	def message2Json(x : MongoDBObject) : JsValue = toJson(Map("method" -> toJson("message"), "sender" -> toJson(getSender(x)), "receiver" -> toJson(getReceiver(x)), "receiver_type" -> toJson(getReceiverType(x)),
								"message_type" -> toJson(getMessageType(x)), "message_content" -> toJson(getMessageContent(x)), 
								"date" -> toJson(getMessageDate(x))))
	
	def queryMessageBase(data : JsValue, queryCondition : DBObject) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
//		val date = (data \ "date").asOpt[String].get

		def target(x : MongoDBObject) : String = {
		  	if (getSender(x) == user_id) getReceiver(x) else getSender(x)
		}
		
		val collection_name = "chat_history"

		val result = from db() in collection_name where queryCondition select (x => x)

		var resultMap : Map[String, List[JsValue]] = Map.empty
		result.toList.distinct map { x =>
			resultMap.get(target(x)).map {
				lt => 
					resultMap += target(x) -> 
						(lt :+ message2Json(x))
			}.getOrElse{
					resultMap += target(x) -> (message2Json(x) :: Nil)
			}
		}
	
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(resultMap)))
	}
	
	def queryMessageWithFriend(data : JsValue) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		val target_id = (data \ "target_id").asOpt[String].get
		queryMessageBase(data, $or($and("sender" -> user_id, "receiver" -> target_id), $and("sender" -> target_id, "receiver" -> user_id)))
	}
	
	def queryMessages(data : JsValue) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		queryMessageBase(data, $or("sender" -> user_id, "receiver" -> user_id))
	}
}