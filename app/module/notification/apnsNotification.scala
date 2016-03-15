package module.notification

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

import com.notnoop.apns.APNS
import java.util.Date
import scala.collection.JavaConversions._
import com.notnoop.apns.EnhancedApnsNotification

object apnsNotification {
	val service = APNS.newService.withCert("certificates/Certificates.p12", "Abcde@196125").withSandboxDestination.build

	def unRegisterUserDevices(user_id : String, device_token : String) = {
		val deviceList = from db() in "devices" where ("user_id" -> user_id) select (x => x)
		if (!deviceList.empty) {
			val dl = deviceList.head
			dl += "devices" -> MongoDBList.newBuilder.result
      _data_connection.getCollection("devices").update(DBObject("user_id" -> user_id), dl)
//			dl.get("devices").map { x =>
//				  	if (x.asInstanceOf[BasicDBList].contains(device_token)) {
//				  		x.asInstanceOf[BasicDBList].remove(device_token)
//				  		_data_connection.getCollection("devices").update(DBObject("user_id" -> user_id), dl)
//				  	}
//				}.getOrElse (throw new Exception)
		}
	}
	
//	def registerUserDevices(data : JsValue) : JsValue = {
	def registerUserDevices(data : JsValue)(cur : MongoDBObject) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val device_token = (data \ "device_token").asOpt[String].map (x => x).getOrElse("")
		
		/** 
		 * uuid
		 */
		val device_uuid = (data \ "device_uuid").asOpt[String].get
		val device_system = (data \ "device_system").asOpt[String].get
	
		def createDevice : MongoDBObject = {
		    val device_builder = MongoDBObject.newBuilder
		    device_builder += "sys" -> device_system
		    device_builder += "uuid" -> device_uuid
		    device_builder += "apns" -> device_token
		
		    device_builder.result
		}
		
		/**
		 * check user is existing or not 
		 * check token is validate or not
		 */
//		(from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
//		    case Nil => ErrorCode.errorToJson("user not existing")
//		    case head :: Nil => {
		        (from db() in "devices" where ("user_id" -> user_id) select (x => x)).toList match {
		            case Nil => {
		                val builder = MongoDBObject.newBuilder
		                builder += "user_id" -> user_id
		                
		                val builder_list = MongoDBList.newBuilder
		                builder_list += createDevice
		                builder += "devices" -> builder_list.result

		                _data_connection.getCollection("devices") += builder.result
		            }
		            case head :: Nil => {
            				head.get("devices").map { x => 
            				    x.asInstanceOf[BasicDBList].toList.filter (iter => iter.asInstanceOf[BasicDBObject].get("sys").equals("ios")) match {
            				        case Nil => x.asInstanceOf[BasicDBList] += createDevice
            				        case hd :: Nil => {
            				            hd.asInstanceOf[DBObject] += "uuid" -> device_uuid
            				            hd.asInstanceOf[DBObject] += "apns" -> device_token
            				        }
            				    }
                      _data_connection.getCollection("devices").update(DBObject("user_id" -> user_id), head)
            				}.getOrElse (throw new Exception)
		            }
		            case _ => ???
		        }
//		    }
//		    case _ => ???
//		}
		
    Json.toJson(Map("status" -> toJson("ok")))
	}
	
	def notificationAll = {

		var deviceList : List[String] = Nil
		(from db() in "devices" select (x => x)).toList.foreach { iter =>
			iter.get("devices").map { lst =>
			  	lst.asInstanceOf[BasicDBList].foreach { device =>
			  	  	deviceList = device.asInstanceOf[BasicDBObject].get("apns").asInstanceOf[String] :: deviceList
			  	}
			}.getOrElse(Unit)
		}

		val payload = APNS.newPayload.alertBody("Alfred Test").build
		deviceList.distinct.foreach { token => 
			service.push(token, payload)
		}
	}
	
	/**
	 * parameters:
	 * 		senderAccount : notificatioin_account
	 *   	receiverType : 0 => User, 1 => ChatGroup, 2 => UserGroup
	 *    	receiverIds	: []
	 *      isSave : 0 => Not Save, 1 => Save
	 *      msgType : 0 => text, 3 => image, 4 => voice
	 *      content : message content
	 *      thumb : 
	 *      voiceLen : null
	 *      pushFormart :
	 *      extraData :
	 */
	def nofity(message : String, to : String, action : Int) = {
	  
		var deviceList : List[String] = Nil
		(from db() in "devices" where ("user_id" -> to) select (x => x)).toList.foreach { iter =>
			iter.get("devices").map { lst =>
			  	lst.asInstanceOf[BasicDBList].foreach { device =>
			  	  	deviceList = device.asInstanceOf[BasicDBObject].get("apns").asInstanceOf[String] :: deviceList
//			  	  	deviceList = device.asInstanceOf[String] :: deviceList
			  	}
			}.getOrElse(Unit)
		}
	  
		val payload = APNS.newPayload.alertBody(message).customField("action", action).build
		deviceList.distinct.foreach { token => 
			service.push(token, payload)
		}
	}
}