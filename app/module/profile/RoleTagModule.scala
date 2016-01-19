package module.profile

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object RoleTagModule {
	def queryAllRoleTags(data : JsValue) : JsValue = {
//		val user_id = (data \ "user_id").asOpt[String].get
//		val auth_token = (data \ "auth_token").asOpt[String].get
		
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(20)
		
		// TODO: check the user_id is valid
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(
		    (from db() in "role_tags").selectSkipTop(skip)(take)("times")( x => 
		      	toJson(x.getAs[String]("tag_name").get)).toList)))
	}
	
	def addRoleTags(data : JsValue) : JsValue = {
	  
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		val tag_name = (data \ "tag_name").asOpt[String].get
	  
		addRoleTags(tag_name)
	}
	
	private def addRoleTags(tag_name : String) : JsValue = {
		val tmp = from db() in "role_tags" where ("tag_name" -> tag_name) select (x =>x)
		if (tmp.empty) {
			val builder = MongoDBObject.newBuilder
			builder += "tag_name" -> tag_name
			builder += "times" -> 1
			_data_connection.getCollection("role_tags") += builder.result
		  
		} else {
			val x = tmp.head
			x += "times" -> new Integer(x.getAs[Int]("times").get + 1)
			_data_connection.getCollection("role_tags").update(DBObject("tag_name" -> tag_name), x)
		}
	
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("add role tag success")))
	}
	
	def deleteRoleTags(data : JsValue) : JsValue = ???
}