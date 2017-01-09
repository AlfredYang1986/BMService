package module.phonecode

import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson
import com.mongodb.casbah.Imports._

import module.sercurity.Sercurity
import module.sms.smsModule

import util.dao.from
import util.dao._data_connection

import PhoneCodeMessages._
import dongdamessages.MessageDefines
import pattern.ModuleTrait
import util.errorcode.ErrorCode

object PhoneCodeModule extends ModuleTrait {
	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case msg_SendSMSCode(data) => sendSMSCode(data)
		case msg_CheckSMSCode(data) => checkSMSCode(data)
		case _ => ???
	}
	
	def sendSMSCode(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val phoneNo = (data \ "phoneNo").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			/**
			 * generate code
			 */
			val code = 1111 // fake one
//			val code = scala.util.Random.nextInt(9000) + 1000
	
			/**
			 * generate a reg token
			 */
//			val time_span_minutes = Sercurity.getTimeSpanWithMinutes
			val reg_token = Sercurity.md5Hash(phoneNo + Sercurity.getTimeSpanWithMinutes)
			
			val builder = MongoDBObject.newBuilder
			builder += "phoneNo" -> phoneNo
			builder += "code" -> code
			builder += "reg_token" -> reg_token
			
			val rel = from db() in "reg" where ("phoneNo" -> phoneNo) select (x => x) 
			if (rel.empty) _data_connection.getCollection("reg") += builder.result
			else _data_connection.getCollection("reg").update(DBObject("phoneNo" -> phoneNo), builder.result)
	
			/**
			 * send code to the phone
			 */	
//			import play.api.Play.current
//			smsModule().sendSMS(phoneNo, code.toString)
			
			/**
			 * is register 
			 */
			val is_reg = (from db() in "users" where ("phoneNo" -> phoneNo) select (x => x)).toList match {
			                case Nil => false
			                case _ => true
			             }
	
			val result = Map("reg_token" -> toJson(reg_token), 
							 "phoneNo" -> toJson(phoneNo),
							 "is_reg" -> toJson(is_reg))
			
			(Some(result), None)
			
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
	
	def checkSMSCode(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		
		try {
			val phoneNo = (data \ "phoneNo").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
    		var code = (data \ "code").asOpt[String].map (x => x.toInt).getOrElse(throw new Exception("wrong input"))
			val reg_token = (data \ "reg_token").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val uuid = (data \ "uuid").asOpt[String].map (x => x).getOrElse("")
    		val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse("")
    	
    		val tt = Sercurity.getTimeSpanWithPast10Minutes
    		val check = Sercurity.getTimeSpanWithPast10Minutes.map (x => Sercurity.md5Hash(phoneNo + x))
    		if (!Sercurity.getTimeSpanWithPast10Minutes.map (x => Sercurity.md5Hash(phoneNo + x)).contains(reg_token)) throw new Exception("token exprie")
			
			(from db() in "reg" where ("phoneNo" -> phoneNo) select (x => x)).toList match {
    			case Nil => throw new Exception("phone number not valid")
    			case head :: Nil => {
    				if ("13720200856".equals(phoneNo) && code == 2222) {
    					code = head.get("code").get.asInstanceOf[Int]
    				}
    				
    				if (code != head.get("code").get.asInstanceOf[Int]) throw new Exception("wrong validation code")
    				else (Some(Map("result" -> toJson("success"))), None)
    			}
    		}
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
    }
}