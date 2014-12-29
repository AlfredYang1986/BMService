package module.login

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import com.mongodb.casbah.commons.MongoDBObject
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object LoginModule {
	def authWithPhone(data : JsValue) : JsValue = {
	
		val phoneNo = (data \ "phoneNo").asOpt[String].get
		
		/**
		 * generate code
		 */
		val code = 11111 // fake one
//		val code = Math.random % 10000.0

		/**
		 * TODO: send code to the phone
		 */
		
		/**
		 * generate a reg token
		 */
		val time_span_minutes = LoginSercurity.getTimeSpanWithMinutes
		val reg_token = LoginSercurity.md5Hash(phoneNo + time_span_minutes)
		
		val builder = MongoDBObject.newBuilder
		builder += "phoneNo" -> phoneNo
		builder += "code" -> code
		builder += "reg_token" -> reg_token
		
		val rel = from db() in "reg" where ("phoneNo" -> phoneNo) select (x => x) 
		if (rel.empty) _data_connection.getCollection("reg") += builder.result
		else _data_connection.getCollection("reg").update(rel.head.asDBObject, builder.result)
	
		/**
		 * return 
		 */
		Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("reg_token" -> toJson(reg_token), "phoneNo" -> toJson(phoneNo)))))
	}

	def authComfirm(data : JsValue) : JsValue = {

		val phoneNo = (data \ "phoneNo").asOpt[String].get
		val code = (data \ "code").asOpt[String].get.toInt
		val reg_token = (data \ "reg_token").asOpt[String].get
		
		val time_span_minutes = LoginSercurity.getTimeSpanWithMinutes
		val reg_token_new = LoginSercurity.md5Hash(phoneNo + time_span_minutes)
		if (!reg_token_new.equals(reg_token)) {
			ErrorCode.errorToJson("token exprie")
		} else {
			val rel = from db() in "reg" where ("phoneNo" -> phoneNo) select (x => x)
			if (rel.empty) 
				ErrorCode.errorToJson("phone number not valid")
			else {
				if (code != rel.head.get("code").get.asInstanceOf[Int])
					ErrorCode.errorToJson("wrong validation code")
				else {
					/**
					 * when login success save user to the client database
					 * 1. if phoneNo is already exist
					 * 2. if not
					 */
					var result = from db() in "users" where ("phoneNo" -> phoneNo) select (x => x)
					if (result.empty) {
						val new_builder = MongoDBObject.newBuilder

						val time_span = LoginSercurity.getTimeSpanWithMillSeconds
						val auth_token = LoginSercurity.md5Hash(phoneNo + time_span)
					
						new_builder  += "auth_token" -> auth_token
						new_builder  += "phoneNo" -> phoneNo
						new_builder  += "email" -> ""
						
						val new_third_builder = MongoDBList.newBuilder
						new_builder  += "third" -> new_third_builder.result
					
						_data_connection.getCollection("users") += new_builder.result

						Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("auth_token" -> toJson(auth_token)))))
					} else {
						val cur = result.head
						cur += "phoneNo" -> phoneNo
						_data_connection.getCollection("users").update(DBObject("auth_token" -> cur.get("auth_token").get), cur)
					  
						val auth_token = result.head.get("auth_token").get.asInstanceOf[String]

						Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("auth_token" -> toJson(auth_token)))))
					}
				} 
			}
		}
	}
	
	def authWithThird(data : JsValue) : JsValue = {

		val auth_token = (data \ "auth_token").asOpt[String].get
		val provide_name = (data \ "provide_name").asOpt[String].get
		val provide_token = (data \ "provide_token").asOpt[String].get
		val provide_email = (data \ "provide_email").asOpt[String].get
		val provide_screen_name = (data \ "provide_screen_name").asOpt[String].get
	 
		val users = from db() in "users" where ("auth_token" -> auth_token) select (x => x)
		if (users.empty)
			ErrorCode.errorToJson("auth token not valid")
		else {
			val user = users.head
			val third_list = user.get("third").get.asInstanceOf[BasicDBList]
			val tmp = third_list.find(x => x.asInstanceOf[BasicDBObject].get("provide_name") ==  provide_name)
			
			tmp match {
			  case Some(x) => {
				  x.asInstanceOf[BasicDBObject]("provide_name") = provide_name
				  x.asInstanceOf[BasicDBObject]("provide_token") = provide_token
				  x.asInstanceOf[BasicDBObject]("provide_email") = provide_email
				  x.asInstanceOf[BasicDBObject]("provide_screen_name") = provide_screen_name

				  _data_connection.getCollection("users").update(DBObject("auth_token" -> auth_token), user)
			  }
			  case None => {
				  val builder = MongoDBObject.newBuilder
			    
				  builder += ("provide_name") -> provide_name
				  builder += ("provide_token") -> provide_token
				  builder += ("provide_email") -> provide_email
				  builder += ("provide_screen_name") -> provide_screen_name
				  third_list += builder.result

				  _data_connection.getCollection("users").update(DBObject("auth_token" -> auth_token), user)
			  }
			}
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("auth_token" -> toJson(auth_token), "connect_result" -> toJson("success")))))
		}
	}

	def connectWithThird(data : JsValue) : JsValue = authWithThird(data)
}