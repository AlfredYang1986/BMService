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
		val post_id = (data \ "post_id").asOpt[String].get
		val owner_id = (data \ "owner_id").asOpt[String].get

		var group_id : Long = -1
		
		def isChatGroupExisting : Boolean = 
		    (from db() in "groups" where ("post_id" -> post_id) select (x => x.getAs[Long]("group_id").get)).toList match {
		        case Nil => false
		        case x : List[Long] => group_id = x.head; true
		    }
		
		def createChatGroupImpl : JsValue = {
			val result = Await.result((ddn ? DDNCreateChatGroup("groupName" -> toJson(group_name))).mapTo[JsValue], timeout.duration)

  		(result \ "status").asOpt[Int].map { status => status match {
  		  case 200 => {		// success
  			  // get group_id
  			  val group_id = ((result \ "entity").asOpt[JsValue].get \ "groupId").asOpt[Long].get
  			  
  			  val builder = MongoDBObject.newBuilder
  			  builder += "group_name" -> group_name
  			  builder += "group_id" -> group_id
  			  builder += "owner_id" -> owner_id 
  			  builder += "post_id" -> post_id
  			  
  			  builder += "post_thumb" -> (from db() in "posts" where ("post_id" -> post_id) select { x => 
                            			      x.getAs[MongoDBList]("items").map (lst => lst.toSeq.filter {iter =>
                            			           iter.asInstanceOf[BasicDBObject].getAs[Int]("type").get == 0}.head).getOrElse(???)
                          	    		  }).toList.head.asInstanceOf[DBObject].get("name")
                          	    		  
  			  builder += "found_date" -> new Date().getTime
  			  builder += "isActived" -> 1
  	
  			  val lst = MongoDBList.newBuilder
  			  lst += user_id
  			  builder += "joiners" -> lst.result
  
  			  ProfileModule.incrementCycleCount(user_id)
  			  _data_connection.getCollection("groups") += builder.result
  
  			  Json.toJson(Map("status" -> toJson("ok"), "result" -> queryGroupsWithID(group_id, user_id)))
  			  			
  		  }
  		  case _ => ErrorCode.errorToJson("create chat group error")		// error or no response
  		}}.getOrElse(ErrorCode.errorToJson("create chat group error"))	    
		}
  			      
		if (isChatGroupExisting) Json.toJson(Map("status" -> toJson("ok"), "result" -> queryGroupsWithID(group_id, user_id)))
		else createChatGroupImpl
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
	
			Json.toJson(Map("status" -> toJson("ok"), "result" -> queryGroupsWithID(group_id, user_id)))
		}
	}
	def joinChatGroup(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_id = (data \ "group_id").asOpt[Long].get
		val joiner_id = user_id
		
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

			Json.toJson(Map("status" -> toJson("ok"), "result" -> queryGroupsWithID(group_id, user_id)))
		}	
	}
	def leaveChatGroup(data : JsValue) : JsValue = {
	  		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val group_id = (data \ "group_id").asOpt[Long].get
		val joiner_id = user_id
		
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

			Json.toJson(Map("status" -> toJson("ok"), "result" -> queryGroupsWithID(group_id, user_id)))
		}	
	}
	
	def delectChatGroup(data : JsValue) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		var group_id = (data \ "group_id").asOpt[Long].get
		
		val rel = from db() in "groups" where ("group_id" -> group_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("group is not exist")
		else {
			val result = Await.result((ddn ? DDNDismissChatGroup("groupId" -> toJson(group_id))).mapTo[JsValue], timeout.duration)

			(result \ "status").asOpt[Int].map { status => status match {
				case 200 => {		// success
				    println("success")
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
	
	def queryGroupsWithID(group_id : Long, user_id : String) : JsValue = {

	    (from db() in "groups" where ("group_id" -> group_id)).selectTop(1)("group_id") { x =>
		  	var tmp : Map[String, JsValue] = Map.empty
  			x.getAs[String]("group_name").map (y => tmp += "group_name" -> toJson(y)).getOrElse(Unit)
	  		x.getAs[String]("owner_id").map (y => tmp += "owner_id" -> toJson(y)).getOrElse(Unit)
		  	x.getAs[Long]("group_id").map (y => tmp += "group_id" -> toJson(y)).getOrElse(Unit)
			  x.getAs[String]("post_id").map (y => tmp += "post_id" -> toJson(y)).getOrElse(Unit)
			  x.getAs[String]("post_thumb").map (y => tmp += "post_thumb" -> toJson(y)).getOrElse(Unit)
			  x.getAs[MongoDBList]("joiners").map { y => 
			  	tmp += "joiners_count" -> toJson(y.length)
			
			  	tmp += "in_the_group" -> {
			  		if (y.exists(iter => iter.asInstanceOf[String].equals(user_id))) toJson(1)
			  		else toJson(0)
			  	}
		  	}.getOrElse(Unit)
		  	toJson(tmp)
		}.head.asInstanceOf[JsValue]
	}
	
	def queryGroups(data : JsValue) : JsValue = {
	  
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get

		/**
		 * 1 => query my group
		 * 0 => query recommend groups
		 */
		val rel = from db() in "groups" select { x =>
		  	var tmp : Map[String, JsValue] = Map.empty
			x.getAs[String]("group_name").map (y => tmp += "group_name" -> toJson(y)).getOrElse(Unit)
			x.getAs[String]("owner_id").map (y => tmp += "owner_id" -> toJson(y)).getOrElse(Unit)
			x.getAs[Long]("group_id").map (y => tmp += "group_id" -> toJson(y)).getOrElse(Unit)
			x.getAs[String]("post_id").map (y => tmp += "post_id" -> toJson(y)).getOrElse(Unit)
			x.getAs[String]("post_thumb").map (y => tmp += "post_thumb" -> toJson(y)).getOrElse(Unit)
			x.getAs[MongoDBList]("joiners").map { y => 
			  	tmp += "joiners_count" -> toJson(y.length)
			  	
			  	tmp += "in_the_group" -> {
			  		if (y.exists(iter => iter.asInstanceOf[String].equals(user_id))) toJson(1)
			  		else toJson(0)
			  	}
			}.getOrElse(Unit)
			toJson(tmp)
		}
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(rel.toList)))
	}
	
	def queryPhotoListAndCount(post_id : String) : (List[JsValue], Int) = {
	    (from db() in "groups" where ("post_id" -> post_id) select { x => 
	        x.getAs[MongoDBList]("joiners").map { y => 
	           (y.toList.take(3).map ( z => z.asInstanceOf[String]), y.count(_ => true))
	        }.getOrElse((Nil, 0))
	    }).toList match {
	        case Nil => (Nil, 0)
	        case (lst, count) :: Nil => {
	            var conditions : DBObject = null
	            lst.foreach ( iter => if (conditions == null) conditions = "user_id" $eq iter
	                                  else conditions = $or("user_id" $eq iter, conditions))
	                                  
	            ((from db() in "user_profile" where conditions select (x => toJson(x.getAs[String]("screen_photo").get))).toList, count)
	        }
	        case _ => ???
	    }
	}
}