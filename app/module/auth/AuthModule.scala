package module.auth

import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson
import com.mongodb.casbah.Imports._

import module.sercurity.Sercurity
import module.sms.smsModule

import util.dao.from
import util.dao._data_connection

import AuthMessage._
import dongdamessages.MessageDefines
import pattern.ModuleTrait

import util.errorcode.ErrorCode

object AuthModule extends ModuleTrait {
	
	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case msg_AuthPhoneCode(data) => authWithPhoneCode(data)
		case msg_AuthThird(data) => authWithThird(data)
		case msg_AuthSignOut(data) => authSignOut(data)
		case msg_AuthQuery(data) => authDataQuery(data)(pr)
		case msg_AuthCheck(data) => authCheck(data)
		case _ => ???
	}

    def authCheck(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val user_id = (data \ "user_id").asOpt[String].get

            (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
                case Nil => throw new Exception("unknown user")
                case head :: Nil => (Some(Map("result" -> toJson("ok"))), None)
            }

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
	
	def authWithPhoneCode(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val phoneNo = (data \ "phoneNo").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val uuid = (data \ "uuid").asOpt[String].map (x => x).getOrElse("")
//			var code = (data \ "code").asOpt[String].get.toInt
//			val reg_token = (data \ "reg_token").asOpt[String].get
			
			(from db() in "users" where ("phoneNo" -> phoneNo) select (x => x)).toList match {
				case Nil => { // not register
					val builder = MongoDBObject.newBuilder
					
					val time_span = Sercurity.getTimeSpanWithMillSeconds
					val user_id = Sercurity.md5Hash(phoneNo + time_span)
					val auth_token = Sercurity.md5Hash(user_id + uuid + time_span)
			
					builder  += "user_id" -> user_id
					builder  += "auth_token" -> auth_token
					builder  += "phoneNo" -> phoneNo
					builder  += "pwd" -> "12345"
					builder  += "email" -> ""
					builder  += "name" -> ""
					builder  += "devices" -> MongoDBList.newBuilder.result
					builder  += "third" -> MongoDBList.newBuilder.result
								
					_data_connection.getCollection("users") += builder.result
					(Some(Map("user_id" -> toJson(user_id),
					    	  "auth_token" -> toJson(auth_token),
					    	  "phoneNo" -> toJson(phoneNo))), None)
				}
				case head :: Nil => { // registered 
					val user_id = head.getAs[String]("user_id").get
					val auth_token = head.getAs[String]("auth_token").get
					(Some(Map("user_id" -> toJson(user_id),
					    	  "auth_token" -> toJson(auth_token),
					    	  "phoneNo" -> toJson(phoneNo))), None)
				}
				case _ => ???
			}
			
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
	
	def authWithThird(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val provide_name = (data \ "provide_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val provide_token = (data \ "provide_token").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val provide_uid = (data \ "provide_uid").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val provide_screen_name = (data \ "provide_screen_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val provide_screen_photo = (data \ "provide_screen_photo").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
		
			val uuid = (data \ "uuid").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
	
			(from db() in "users" where ("third.provide_name" -> provide_name, "third.provide_uid" -> provide_uid) select (x => x)).toList match {
				case Nil => {
					val new_builder = MongoDBObject.newBuilder
				
					val time_span = Sercurity.getTimeSpanWithMillSeconds
					val user_id = Sercurity.md5Hash(provide_name + provide_token + time_span)
					val auth_token = Sercurity.md5Hash(user_id + uuid + time_span)
				
					new_builder  += "user_id" -> user_id
					new_builder  += "auth_token" -> auth_token
					new_builder  += "phoneNo" -> ""
					new_builder  += "email" -> ""
					new_builder  += "name" -> provide_screen_name
					new_builder  += "pwd" -> "12345"
					new_builder  += "devices" -> MongoDBList.newBuilder.result
					
					val new_third_builder = MongoDBList.newBuilder
			
					val builder_third = MongoDBObject.newBuilder
					builder_third += ("provide_name") -> provide_name
					builder_third += ("provide_token") -> provide_token
					builder_third += ("provide_uid") -> provide_uid
					builder_third += ("provide_screen_name") -> provide_screen_name
					builder_third += ("provide_screen_photo") -> provide_screen_photo
			
					new_third_builder += builder_third.result
					
					new_builder  += "third" -> new_third_builder.result
				 
					_data_connection.getCollection("users") += new_builder.result
					(Some(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token), "screen_name" -> toJson(provide_screen_name))), None)
				}
				case user :: Nil => {
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
							  x.asInstanceOf[BasicDBObject] += ("provide_screen_photo") -> provide_screen_photo
			
							  _data_connection.getCollection("users").update(DBObject("user_id" -> user_id), user)
						}
						
						case None => {
							  val builder = MongoDBObject.newBuilder
						    
							  builder += ("provide_name") -> provide_name
							  builder += ("provide_token") -> provide_token
							  builder += ("provide_uid") -> provide_uid
							  builder += ("provide_screen_name") -> provide_screen_name
							  builder += ("provide_screen_photo") -> provide_screen_photo
							  third_list += builder.result
			
							  _data_connection.getCollection("users").update(DBObject("user_id" -> user_id), user)
						}
					}
					
					(Some(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token), "screen_name" -> toJson(provide_screen_name))), None)
				}
				case _ => ???
			}
				
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
	
	def authSignOut(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val auth_token = (data \ "auth_token").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val device_token = (data \ "device_token").asOpt[String].map (x => x).getOrElse("")
	
			(Some(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token) )), None)
			
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
	
	def authDataQuery(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse { pr match {
				case None => throw new Exception("wrong input")
				case Some(m) => m.get("user_id").map (x => x.asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))).getOrElse(throw new Exception("wrong input"))
			}}
			
			(from db() in "users" where ("user_id" -> user_id) select(x => x)).toList match {
				case head :: Nil => pr match {
					case None => throw new Exception("wrong input")
					case Some(m) => {
						var npr = m + ("is_real_name_cert" -> toJson(head.getAs[MongoDBObject]("real_name").map { x => 
															              x.getAs[Number]("status").get.intValue == 1
															          }.getOrElse(false)))

						if (m.get("has_phone").get.asOpt[Boolean].get == false) 
							npr = npr + ("has_phone" -> toJson(head.getAs[String]("phoneNo").map (x => x.length > 0).getOrElse(false)))
					
						(Some(npr), None)
					}
				}
				case _ => throw new Exception("wrong input")
			}
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
}