package module.login

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import module.sms._
import module.relationship._
import module.profile.ProfileModule
import module.notification._

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef

object phoneCheckModule {
    def pushSMSCode(data : JsValue) : JsValue = {
        val phoneNo = (data \ "phoneNo").asOpt[String].get
		
		    val code = 1111 // fake on
//  		val code = scala.util.Random.nextInt(9000) + 1000

	    	/**
		 		 * generate a reg token
		 		 */
		    val time_span_minutes = Sercurity.getTimeSpanWith10Minutes
		    val reg_token = Sercurity.md5Hash(phoneNo + time_span_minutes)
		
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
//  		import play.api.Play.current
//  		smsModule().sendSMS(phoneNo, code.toString)
		
	    	/**
		 		 * is register 
				 */
		    val is_reg = (from db() in "users" where ("phoneNo" -> phoneNo) select (x => x)).toList match {
		                case Nil => false
		                case _ => true
		             }
		
		    Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				     toJson(Map("reg_token" -> toJson(reg_token), 
				                "phoneNo" -> toJson(phoneNo),
				                "is_reg" -> toJson(is_reg)))))   
    }
    
    def checkSMSCode(data : JsValue) : JsValue = {
        val phoneNo = (data \ "phoneNo").asOpt[String].get
    		val uuid = (data \ "uuid").asOpt[String].map (x => x).getOrElse("")
    		val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse("")
    		val code = (data \ "code").asOpt[String].get.toInt
		    val reg_token = (data \ "reg_token").asOpt[String].get
		
    		val time_span_minutes = Sercurity.getTimeSpanWith10Minutes
		    val reg_token_new = Sercurity.md5Hash(phoneNo + time_span_minutes)
		
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
				            if (!user_id.isEmpty())
				                ProfileModule.updateUserProfile(toJson(Map("user_id" -> toJson(user_id), "contact_no" -> toJson(phoneNo))))
				              
				            toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
                }
			      }
		    }
    }
}