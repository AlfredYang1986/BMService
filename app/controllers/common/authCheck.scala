package controllers.common

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import util.dao._
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

class authCheck {
   
  	def apply(data : JsValue)(impl : JsValue => (MongoDBObject => JsValue)) : JsValue = {
   	    val user_id = (data \ "user_id").asOpt[String].get
   	    val auth_token = (data \ "auth_token").asOpt[String].get
   	  
   	    val a : Int => (Int => Int) = null
   	    
   	    (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
   	        case Nil => ErrorCode.errorToJson("user not existing")
   	        case head :: Nil => {
   	            if (auth_token.equals(head.getAs[String]("auth_token").get)) impl(data)(head)
   	            else ErrorCode.errorToJson("token not valid")
   	        }
   	        case _ => ???
   	    }
   	} 
}