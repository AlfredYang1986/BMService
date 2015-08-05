package module.relationship

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import java.util.Date

import module.common.helpOptions
import scala.collection.JavaConversions._

object RelationshipModule {
 
	def follow(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val follow_user_id = (data \ "follow_user_id").asOpt[String].get

		def addfollowings(owner : String, follower : String) : Boolean = {
			val lst = from db() in "relationship" where ("user_id" -> owner) select (x => x)

			if (lst.count == 0) {
				val builder = MongoDBObject.newBuilder
				
				val following_lst = MongoDBList.newBuilder
				following_lst += follower
				
				val followed_lst = MongoDBList.newBuilder
				
				builder += "user_id" -> owner
				builder += "following" -> following_lst.result
				builder += "followed" -> followed_lst.result
	
				_data_connection.getCollection("relationship") += builder.result
				true
			}
			else if (lst.count == 1) {
				val user = lst.head
				val following = user.getAs[MongoDBList]("following").get

				if (!following.exists(x => x.asInstanceOf[String].equals(follower))) {
					following.add(follower)
					_data_connection.getCollection("relationship").update(DBObject("user_id" -> owner), user)
					true
				}
			}
			false
		}
	
		def addfolloweds(owner : String, followed : String) : Boolean = {
			val lst = from db() in "relationship" where ("user_id" -> owner) select (x => x)

			if (lst.count == 0) {
				val builder = MongoDBObject.newBuilder
				
				val following_lst = MongoDBList.newBuilder
				
				val followed_lst = MongoDBList.newBuilder
				followed_lst += followed
				  
				builder += "user_id" -> owner
				builder += "following" -> following_lst.result
				builder += "followed" -> followed_lst.result
	
				_data_connection.getCollection("relationship") += builder.result
				true
			}
			else if (lst.count == 1) {
				val user = lst.head
				val following = user.getAs[MongoDBList]("followed").get
				if (!following.exists(x => (x.asInstanceOf[String]).equals(followed))) {
					following.add(followed)
					_data_connection.getCollection("relationship").update(DBObject("user_id" -> owner), user)
					true
				}
			}
			false
		}
		

		addfollowings(user_id, follow_user_id)
		addfolloweds(follow_user_id, user_id)
		
//		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("follow user success")))
		queryMutureFollowingUsers(data)
	}

	def unfollow(data : JsValue) : JsValue = {
			
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val follow_user_id = (data \ "follow_user_id").asOpt[String].get

		def removefollowings(owner : String, follower : String) : Boolean = {
			val lst = from db() in "relationship" where ("user_id" -> owner) select (x => x)

			if (lst.count == 1) {
				val user = lst.head
				val following = user.getAs[MongoDBList]("following").get
				if (following.exists(x => (x.asInstanceOf[String]).equals(follower))) {
					following.remove(follower)
					_data_connection.getCollection("relationship").update(DBObject("user_id" -> owner), user)
					true
				}
			}
			false
		}
	
		def removefolloweds(owner : String, followed : String) : Boolean = {
			val lst = from db() in "relationship" where ("user_id" -> owner) select (x => x)

			if (lst.count == 1) {
				val user = lst.head
				val followed_lst = user.getAs[MongoDBList]("followed").get
				if (followed_lst.exists(x => (x.asInstanceOf[String]).equals(followed))) {
					followed_lst.remove(followed)
					println(followed_lst)
					println(user)
					_data_connection.getCollection("relationship").update(DBObject("user_id" -> owner), user)
					true
				}
			}
			false
		}

		removefollowings(user_id, follow_user_id)
		removefolloweds(follow_user_id, user_id)
		
//		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("follow user success")))
		queryMutureFollowingUsers(data)
	}

	def queryFollowingUsers(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val owner_id = (data \ "owner_id").asOpt[String].get
		
		val lst = from db() in "relationship" where ("user_id" -> owner_id) select (x => x)
		var tmp : Map[String, JsValue] = Map.empty
		if (lst.count == 0) {
		  	tmp += "following" -> null
		} else if (lst.count == 1) {
			tmp = this.queryResult(lst.head, List("following"))
		  
		} else {
			ErrorCode.errorToJson("unknown error")
		}
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
	}

	def queryFollowedUsers(data : JsValue) : JsValue = {
 
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val owner_id = (data \ "owner_id").asOpt[String].get
		
		val lst = from db() in "relationship" where ("user_id" -> owner_id) select (x => x)
		var tmp : Map[String, JsValue] = Map.empty
		if (lst.count == 0) {
		  	tmp += "followed" -> null
		} else if (lst.count == 1) {
			tmp = this.queryResult(lst.head, List("followed"))
		  
		} else {
			ErrorCode.errorToJson("unknown error")
		}
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
	}

	def queryMutureFollowingUsers(data : JsValue) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val owner_id = (data \ "owner_id").asOpt[String].get
	
		val lst = from db() in "relationship" where ("user_id" -> owner_id) select (x => x)
		var tmp : Map[String, JsValue] = Map.empty
		if (lst.count == 0) {
		  	tmp += "followed" -> null
		  	tmp += "following" -> null
		} else if (lst.count == 1) {
		  tmp = this.queryResult(lst.head, List("followed", "following"))
		  
		} else {
			ErrorCode.errorToJson("unknown error")
		}
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
	}
	
	def queryResult(one : MongoDBObject, lst : List[String]) : Map[String, JsValue] = {
		var tmp : Map[String, JsValue] = Map.empty
		lst map { iter => 
			var tmp_lst : List[String] = Nil
		  	one.getAs[MongoDBList](iter).map (value => 
		  		value.foreach (x => tmp_lst = x.asInstanceOf[String] :: tmp_lst)).
		  		getOrElse(Unit)
		  	
		  	tmp += iter -> toJson(tmp_lst)
		}
		tmp
	}
}