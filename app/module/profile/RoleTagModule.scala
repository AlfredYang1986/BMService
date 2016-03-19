package module.profile

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import java.util.Date

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
	
//	def addRoleTags(data : JsValue) : JsValue = {
	def addRoleTags(data : JsValue)(cur : MongoDBObject) : JsValue = {
	  
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
	
//	def queryRoleTagPreViewWithRoleTag(data : JsValue) : JsValue = {
	def queryRoleTagPreViewWithRoleTag(data : JsValue)(cur : MongoDBObject) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val role_tag = (data \ "role_tag").asOpt[String].get
		val date = (data \ "date").asOpt[Long].map(x => x).getOrElse(new Date().getTime)	    
		
	  def getPreViewRoleTag(o : MongoDBObject) : String = o.getAs[String]("role_tag").map (x => x).getOrElse("")
	 
	  def queryPreViewWithRoleTag(tag : String) : JsValue = {
  		(from db() in "user_profile" where ("role_tag" -> tag)).selectTop(3)("date") { x =>
  		    toJson(Map(
  		            "user_id" -> toJson(x.getAs[String]("user_id").get),
  		            "screen_name" -> toJson(x.getAs[String]("screen_name").get),
  		            "screen_photo" -> toJson(x.getAs[String]("screen_photo").get)))
  		}.toList
  		match {
  		    case Nil => null
  		    case x : List[JsValue] => toJson(Map("role_tag" -> toJson(tag), "content" -> toJson(x)))
  		    case _ => null
  		}
		}
		
//		val user_check = from db() in "users" where ("user_id" -> user_id) select (x => x)
//		if (user_check.count == 0) ErrorCode.errorToJson("user not existing")
//		else {
		  var result : List[JsValue] = Nil
      val tag = ((from db() in "role_tags" where ("tag_name" $regex (role_tag))).select(x => x.getAs[String]("tag_name").get)).toList
      tag.map (x => result = (queryPreViewWithRoleTag(x) :: Nil).filterNot(_ == null) ::: result)
			toJson(Map("status" -> toJson("ok"), "preview" -> toJson(result)))
//		}
  }
}