package module.groups

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import java.util.Date
import module.common.helpOptions

import scala.collection.JavaConversions._

object GroupModule2 {
	def createChatGroup(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_name = (data \ "group_name").asOpt[String].get
	
		val rel = (from db() in "groups" where ("isActived" -> 0)).selectTop(1)("grouop_id")(x => x)
		
		var group_id : Long = 0
		if (rel.empty) {
			group_id = (from db() in "groups").count.toLong
		
			val builder = MongoDBObject.newBuilder
			builder += "group_name" -> group_name
			builder += "group_id" -> group_id
			builder += "owner_id" -> user_id
			builder += "found_date" -> new Date().getTime
			builder += "isActived" -> 1
			
			builder += "joiners" -> MongoDBList.newBuilder.result

			_data_connection.getCollection("groups") += builder.result
		} else {
		  
			val group = rel.head
			group.getAs[Long]("group_id").map (x => group_id = x).getOrElse(throw new Exception("error"))
			group += "group_name" -> group_name
			group += "owner_id" -> user_id
			group += "found_date" -> new Date().getTime.asInstanceOf[AnyRef]
			group += "isActived" -> new Integer(1)

			group += "joiners" -> MongoDBList.newBuilder.result

			_data_connection.getCollection("groups").update(DBObject("group_id" -> group_id), group)
		}
	
		Json.toJson(Map("status" -> toJson("ok"), "result" -> 
			toJson(Map("group_id" -> toJson(group_id), "group_name" -> toJson(group_name)))))
	}
	
	def updateChatGroup(data : JsValue) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_id = (data \ "group_id").asOpt[Long].get
		
		val rel = from db() in "groups" where ("group_id" -> group_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("group is not exist")
		else {
			val group = rel.head
			
			(data \ "group_name").asOpt[String].map(x => group += "group_name" -> x).getOrElse(Unit)
			_data_connection.getCollection("groups").update(DBObject("group_id" -> group_id), group)

			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("update success")))
		}
	}
	
	def joinChatGroup(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_id = (data \ "group_id").asOpt[Long].get
		val joiner_id = (data \ "joiner_id").asOpt[String].get
		
		val rel = from db() in "groups" where ("group_id" -> group_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("group is not exist")
		else {
			val group = rel.head

			group.getAs[MongoDBList]("joiners").map { x => 
			 	val lst = x.asInstanceOf[MongoDBList]
			  	if (!lst.exists(iter => iter.asInstanceOf[String].equals(joiner_id))) {
			  		lst.add(joiner_id)
					_data_connection.getCollection("groups").update(DBObject("group_id" -> group_id), group)
			  	}
			  
			}.getOrElse(Unit)

			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("join group success")))
		}	
	}
	
	def leaveChatGroup(data : JsValue) : JsValue = {
	  		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_id = (data \ "group_id").asOpt[Long].get
		val joiner_id = (data \ "joiner_id").asOpt[String].get
		
		val rel = from db() in "groups" where ("group_id" -> group_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("group is not exist")
		else {
			val group = rel.head

			group.getAs[MongoDBList]("joiners").map { x => 
			 	val lst = x.asInstanceOf[MongoDBList]
			  	if (lst.exists(iter => iter.asInstanceOf[String].equals(joiner_id))) {
			  		lst.remove(joiner_id)
					_data_connection.getCollection("groups").update(DBObject("group_id" -> group_id), group)
			  	}
			  
			}.getOrElse(Unit)

			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("leave group success")))
		}	
	}
	
	def delectChatGroup(data : JsValue) : JsValue = {
				
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_id = (data \ "group_id").asOpt[Long].get
		
		val rel = from db() in "groups" where ("group_id" -> group_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("group is not exist")
		else {
			val group = rel.head

			group += "isActived" -> 0.asInstanceOf[AnyRef]

			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("dissmiss group success")))
		}	
	}
	
	def queryGroups(data : JsValue) : JsValue = {
	  
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get

		val rel = from db() in "groups" select { x =>
		  	var tmp : Map[String, JsValue] = Map.empty
			x.getAs[String]("group_name").map (y => tmp += "group_name" -> toJson(y)).getOrElse(Unit)
			x.getAs[Long]("group_id").map (y => tmp += "group_id" -> toJson(y)).getOrElse(Unit)
			x.getAs[MongoDBList]("joiners").map (y => tmp += "joiners_count" -> toJson(y.length)).getOrElse(Unit)
			toJson(tmp)
		}
		
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(rel.toList)))
	}
}