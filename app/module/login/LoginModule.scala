package module.login

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object LoginModule {
 	
	def isAuthTokenValidate(token : String) : Boolean = !((from db() in "users" where ("auth_token" -> token) select (x => x)).empty)
	def isUserExist(user_id : String) : Boolean = !((from db() in "users" where ("user_id" -> user_id) select (x => x)).empty)
	
	def authUpdateDetails(data : JsValue) : JsValue = {
		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id= (data \ "user_id").asOpt[String].get
	
		val rel = from db() in "users" where ("user_id" -> user_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("auth token not valid")
		else {
			val user = rel.head
			List("name", "phoneNo", "email") foreach { x => 
				(data \ x).asOpt[String].map { value =>
				  user += x -> value
				}.getOrElse(Unit)
			}
			_data_connection.getCollection("users").update(DBObject("user_id" -> user_id), user)
			
			Json.toJson(Map("status" -> toJson("ok")))
		}
	}
  
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
		else _data_connection.getCollection("reg").update(DBObject("phoneNo" -> phoneNo), builder.result)
	
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
					 */
					var result = from db() in "users" where ("phoneNo" -> phoneNo) select (x => x)
					if (result.empty) {
						
						/**
						 * 1. this phone is not reg
						 * 		create a new auth_token and connect to this phone number
						 */
						this.authCreateNewUserWithPhone(phoneNo)

					} else {
					  	/**
						 * 2. this phone is already reg
						 * 		pass this token to the client
						 */
						val cur = result.head
//						cur += "phoneNo" -> phoneNo
//						_data_connection.getCollection("users").update(DBObject("auth_token" -> cur.get("auth_token").get), cur)
					  
						val auth_token = cur.get("auth_token").get.asInstanceOf[String]
						val user_id = cur.get("user_id").get.asInstanceOf[String]
						val name = cur.get("name").get.asInstanceOf[String]

						Json.toJson(Map("status" -> toJson("error"), "error" -> 
							toJson(Map("message" -> toJson("already login"), 
							    "auth_token" -> toJson(auth_token), 
							    "user_id" -> toJson(user_id), 
							    "name" -> toJson(name),
							    "phoneNo" -> toJson(phoneNo)))))
					}
				} 
			}
		}
	}
	
	private def createNewUserWithProviderDetails(provide_name: String, provide_token: String, provide_uid: String, provide_screen_name: String) : JsValue = {
		val new_builder = MongoDBObject.newBuilder

		val time_span = LoginSercurity.getTimeSpanWithMillSeconds
		val user_id = LoginSercurity.md5Hash(provide_name + provide_token + time_span)
		val auth_token = LoginSercurity.md5Hash(provide_name + provide_token + time_span)
					
		new_builder  += "user_id" -> user_id
		new_builder  += "auth_token" -> auth_token
		new_builder  += "phoneNo" -> ""
		new_builder  += "email" -> ""
		new_builder  += "name" -> provide_screen_name
		
		val new_third_builder = MongoDBList.newBuilder

		val builder_third = MongoDBObject.newBuilder
		builder_third += ("provide_name") -> provide_name
		builder_third += ("provide_token") -> provide_token
		builder_third += ("provide_uid") -> provide_uid
		builder_third += ("provide_screen_name") -> provide_screen_name

		new_third_builder += builder_third.result
		
		new_builder  += "third" -> new_third_builder.result
	 
	
		_data_connection.getCollection("users") += new_builder.result
		
		Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token), "name" -> toJson(provide_name)))))
	}

	private def connectUserWithProviderDetails(user: MongoDBObject, provide_name: String, provide_token: String, provide_uid: String, provide_screen_name: String) : JsValue = {

		val auth_token = user.get("auth_token").get.asInstanceOf[String]
		val user_id = user.get("user_id").get.asInstanceOf[String]
		val third_list = user.get("third").get.asInstanceOf[BasicDBList]
		var name = user.get("name").get.asInstanceOf[String]
			if (name == "") {
				name = provide_name
				user += ("name") -> name
			}
			val tmp = third_list.find(x => x.asInstanceOf[BasicDBObject].get("provide_name") ==  provide_name)
			
			tmp match {
			  case Some(x) => {
				  x.asInstanceOf[BasicDBObject] += ("provide_name") -> provide_name
				  x.asInstanceOf[BasicDBObject] += ("provide_token") -> provide_token
				  x.asInstanceOf[BasicDBObject] += ("provide_uid") -> provide_uid
				  x.asInstanceOf[BasicDBObject] += ("provide_screen_name") -> provide_screen_name

				  _data_connection.getCollection("users").update(DBObject("auth_token" -> auth_token), user)
			  }
			  case None => {
				  val builder = MongoDBObject.newBuilder
			    
				  builder += ("provide_name") -> provide_name
				  builder += ("provide_token") -> provide_token
				  builder += ("provide_uid") -> provide_uid
				  builder += ("provide_screen_name") -> provide_screen_name
				  third_list += builder.result

				  _data_connection.getCollection("users").update(DBObject("auth_token" -> auth_token), user)
			  }
			}
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token), "name" -> toJson(name), "connect_result" -> toJson("success")))))
	}
	
	def authWithThird(data : JsValue) : JsValue = {

//		val user_id = (data \ "user_id").asOpt[String].get
//		val auth_token = (data \ "auth_token").asOpt[String].get
		val provide_name = (data \ "provide_name").asOpt[String].get
		val provide_token = (data \ "provide_token").asOpt[String].get
		val provide_uid = (data \ "provide_uid").asOpt[String].get
		val provide_screen_name = (data \ "provide_screen_name").asOpt[String].get
  
		val users = from db() in "users" where ("third.provide_name" -> provide_name, "third.provide_uid" -> provide_uid) select (x => x)
	
		if (users.empty) this.createNewUserWithProviderDetails(provide_name, provide_token, provide_uid, provide_screen_name)
		else  this.connectUserWithProviderDetails(users.head, provide_name, provide_token, provide_uid, provide_screen_name)
	}

	def connectWithThird(data : JsValue) : JsValue = authWithThird(data)
	
	def authCreateUserWithPhone(data : JsValue) : JsValue = {

	  val phoneNo = (data \ "phoneNo").asOpt[String].get

		val users = from db() in "users" where ("phoneNo" -> phoneNo) select (x => x)
		if (users.empty) {
			/**
			 * 2. if phoneNo is not, then create one directly
			 */ 
		  	authCreateNewUserWithPhone(phoneNo)
			
		} else {
			/**
			 * 1. if phoneNo is already connect to the auth token
			 * 		unbind the auth token
			 *   	then create new one
			 */	  
			val user = users.head
			user += "phoneNo" -> ""
			_data_connection.getCollection("users").update(DBObject("phoneNo" -> phoneNo), user)
		
			this.authCreateNewUserWithPhone(phoneNo)
		}
	}
	
	private def authCreateNewUserWithPhone(phoneNo : String) : JsValue = {
		val new_builder = MongoDBObject.newBuilder

		val time_span = LoginSercurity.getTimeSpanWithMillSeconds
		val user_id = LoginSercurity.md5Hash(phoneNo + time_span)
		val auth_token = LoginSercurity.md5Hash(user_id + time_span)
					
		new_builder  += "user_id" -> user_id
		new_builder  += "auth_token" -> auth_token
		new_builder  += "phoneNo" -> phoneNo
		new_builder  += "email" -> ""
		new_builder  += "name" -> ""
						
		val new_third_builder = MongoDBList.newBuilder
		new_builder  += "third" -> new_third_builder.result
					
		_data_connection.getCollection("users") += new_builder.result

		Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("user_id" -> toJson(user_id), "phoneNo" -> toJson(phoneNo), "auth_token" -> toJson(auth_token)))))
	}
	

}