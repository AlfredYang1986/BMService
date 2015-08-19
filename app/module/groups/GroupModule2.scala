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
import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{ toJson }
import play.api.libs.json.{ JsValue, JsObject, JsString, JsArray }
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef
import module.notification.{ DDNActor, DDNCreateChatGroup, DDNDismissChatGroup }
import scala.concurrent.Future
import scala.concurrent.Await

import module.profile.ProfileModule

object GroupModule2 {

	val ddn = Akka.system(play.api.Play.current).actorOf(Props[DDNActor])
	implicit val timeout = Timeout(30 second)

	def createChatGroup(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_name = (data \ "group_name").asOpt[String].get

		val result = Await.result((ddn ? DDNCreateChatGroup("roomName" -> toJson(group_name))).mapTo[JsValue], timeout.duration)

		(result \ "status").asOpt[Int].map { status => status match {
		  case 200 => {		// success
			  // get group_id
			  val group_id = ((result \ "entity").asOpt[JsValue].get \ "roomId").asOpt[Long].get
			  
			  val builder = MongoDBObject.newBuilder
			  builder += "group_name" -> group_name
			  builder += "group_id" -> group_id
			  builder += "owner_id" -> user_id
			  builder += "found_date" -> new Date().getTime
			  builder += "isActived" -> 1
	
			  val lst = MongoDBList.newBuilder
			  lst += user_id
			  builder += "joiners" -> lst.result

			  ProfileModule.incrementCycleCount(user_id)
			  _data_connection.getCollection("groups") += builder.result

			  Json.toJson(Map("status" -> toJson("ok"), "result" -> 
			  			toJson(Map("group_id" -> toJson(group_id), "group_name" -> toJson(group_name))))) 
		  }
		  case _ => ErrorCode.errorToJson("create chat group error")		// error or no response
		}}.getOrElse(ErrorCode.errorToJson("create chat group error"))
	}
	
	def updateChatGroup(data : JsValue) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_id = (data \ "group_id").asOpt[Long].get
		val group_name = (data \ "group_name").asOpt[String].get

		val rel = from db() in "groups" where ("group_id" -> group_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("group is not exist")
		else {
			val group = rel.head
				  
			(data \ "group_name").asOpt[String].map(x => group += "group_name" -> x).getOrElse(Unit)
			_data_connection.getCollection("groups").update(DBObject("group_id" -> group_id), group)
	
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				  			toJson(Map("group_id" -> toJson(group_id), "group_name" -> toJson(group_name))))) 
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
			  		ProfileModule.incrementCycleCount(user_id)
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
			  		ProfileModule.decrementCycleCount(user_id)
					_data_connection.getCollection("groups").update(DBObject("group_id" -> group_id), group)
			  	}
			  
			}.getOrElse(Unit)

			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("leave group success")))
		}	
	}
	
	def delectChatGroup(data : JsValue) : JsValue = {
				
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		var group_id = (data \ "group_id").asOpt[Long].get
		
		val rel = from db() in "groups" where ("group_id" -> group_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("group is not exist")
		else {
		 
			val result = Await.result((ddn ? DDNDismissChatGroup("roomId" -> toJson(group_id))).mapTo[JsValue], timeout.duration)

			println(result)
			(result \ "status").asOpt[Int].map { status => status match {
				case 200 => {		// success
					val group = rel.head
		 			group.getAs[MongoDBList]("joiners").map ( x => 
		 				x.asInstanceOf[MongoDBList].foreach (iter => ProfileModule.decrementCycleCount(iter.asInstanceOf[String]))
		 			).getOrElse(Unit)
					
					_data_connection.getCollection("groups").remove(DBObject("group_id" -> group_id))
					Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("dissmiss group success")))
				}
				case _ => ErrorCode.errorToJson("dismiss chat group error")		// error or no response
			}}.getOrElse(ErrorCode.errorToJson("dismiss chat group error"))
		}	
	}
	
	def queryGroups(data : JsValue) : JsValue = {
	  
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get

		/**
		 * 1. query my group
		 */
		val rel = from db() in "groups" where ("joiner" -> user_id) select { x =>
		  	var tmp : Map[String, JsValue] = Map.empty
			x.getAs[String]("group_name").map (y => tmp += "group_name" -> toJson(y)).getOrElse(Unit)
			x.getAs[Long]("group_id").map (y => tmp += "group_id" -> toJson(y)).getOrElse(Unit)
			x.getAs[MongoDBList]("joiners").map (y => tmp += "joiners_count" -> toJson(y.length)).getOrElse(Unit)
			toJson(tmp)
		}
		
		/**
		 * 2. query recommend groups
		 */
		val recommends = from db() in "groups" select { x =>
		  	var tmp : Map[String, JsValue] = Map.empty
			x.getAs[String]("group_name").map (y => tmp += "group_name" -> toJson(y)).getOrElse(Unit)
			x.getAs[Long]("group_id").map (y => tmp += "group_id" -> toJson(y)).getOrElse(Unit)
			x.getAs[MongoDBList]("joiners").map (y => tmp += "joiners_count" -> toJson(y.length)).getOrElse(Unit)
			toJson(tmp)
		}
		
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("my_group" -> toJson(rel.toList), "recommend_group" -> toJson(recommends.toList)))))
	}
}