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
	def registerUserDevices(data : JsValue) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val device_token = (data \ "device_token").asOpt[String].map (x => x).getOrElse("")

		/**
		 * check user is existing or not 
		 * check token is validate or not
		 */
		val rel = from db() in "users" where ("user_id" -> user_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("user not existing")
		else {
			val deviceList = from db() in "devices" where ("user_id" -> user_id) select (x => x)
			if (deviceList.empty) {
				val builder = MongoDBObject.newBuilder
				builder += "user_id" -> user_id
				
				val builder_list = MongoDBList.newBuilder
				if (device_token != "")	builder_list += device_token
				builder += "devices" -> builder_list.result
				
				_data_connection.getCollection("devices") += builder.result
				
			} else {
				val dl = deviceList.head
				dl.get("devices").map { x => 
				  	if (!x.asInstanceOf[BasicDBList].contains(device_token)) {
				  		val builder_list = x.asInstanceOf[BasicDBList]
				  		if (device_token != "")	builder_list += device_token
				  		dl += "devices" -> builder_list
				  		_data_connection.getCollection("devices").update(DBObject("user_id" -> user_id), dl)
				  	}
				}.getOrElse (throw new Exception)
			}

			Json.toJson(Map("status" -> toJson("ok")))
		}
	}
	
	def notificationAll(data : JsValue) : JsValue = {
		val token = "3450fbe83072d7ae155b29c22c2ae170b3bd0ac5f569f837ab6ad0976308febb"
		val service = APNS.newService.withCert("/Users/yangyuan/Desktop/BabySharing2/certificates/Certificates.p12", "Abcde@196125").withSandboxDestination.build
		val payload = APNS.newPayload.alertBody("Alfred Test").build
		service.push(token, payload)
		service.stop()
		
		null
	}
}