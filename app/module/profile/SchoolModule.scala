package module.profile

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object SchoolModule {
	def queryAllSchools(data : JsValue) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(20)
		
		// TODO: check the user_id is valid
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(
		    (from db() in "schools").selectSkipTop(skip)(take)("times")( x => 
		      	toJson(x.getAs[String]("school_name").get)).toList)))
	}
	
	def addOneSchool(data : JsValue) : JsValue = {
	  
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		val school_name = (data \ "school_name").asOpt[String].get
	  
		addOneSchool(school_name)
	}
	
	private def addOneSchool(school_name : String) : JsValue = {
		val tmp = from db() in "schools" where ("school_name" -> school_name) select (x =>x)
		if (tmp.empty) {
			val builder = MongoDBObject.newBuilder
			builder += "school_name" -> school_name
			_data_connection.getCollection("schools") += builder.result
		  
		} else {
//			val x = tmp.head
//			x += "times" -> new Integer(x.getAs[Int]("times").get + 1)
//			_data_connection.getCollection("schools").update(DBObject("school_name" -> school_name), x)
		}
	
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("add role school success")))
	}
	
	def deleteOneSchool(data : JsValue) : JsValue = ???
}