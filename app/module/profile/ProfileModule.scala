package module.profile

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.common.helpOptions
import module.relationship._

import akka.actor.{Actor, Props}
import play.api.libs.concurrent.Akka
import play.api.GlobalSettings
import play.api.templates.Html
import play.api.libs.concurrent.Execution.Implicits.defaultContext

object ProfileModule {

	def like(data : JsValue) : JsValue = {
		null
	}

	/**
	 * update user profile, call by client
	 */
	def updateUserProfile(data : JsValue) : JsValue= {
		val user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse("")
		val screen_name = (data \ "screen_name").asOpt[String].map(x => x).getOrElse("")
		val screen_photo = (data \ "screen_photo").asOpt[String].map(x => x).getOrElse("")
		val role_tag = (data \ "role_tag").asOpt[String].map(x => x).getOrElse("")
		
		if (user_id == "") ErrorCode.errorToJson("user not existing")
		else {
			val reVal = from db() in "user_profile" where ("user_id" -> user_id) select (x => x)
			if (reVal.empty) {
				val builder = MongoDBObject.newBuilder
				builder += "user_id" -> user_id
				builder += "screen_name" -> screen_name
				builder += "screen_photo" -> screen_photo
				builder += "role_tag" -> role_tag
				builder += "followings_count" -> (data \ "followings_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "followers_count" -> (data \ "followers_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "friends_count" -> (data \ "friends_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "posts_count" -> (data \ "posts_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "cycle_count" -> (data \ "cycle_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "isLogin" -> (data \ "isLogin").asOpt[Int].map(x => x).getOrElse(0)
				builder += "signature" -> (data \ "signature").asOpt[String].map(x => x).getOrElse(Unit)
				
				_data_connection.getCollection("user_profile") += builder.result
				Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("user_id" -> user_id, "name" -> screen_name, "screen_photo" -> screen_photo, "role_tag" -> role_tag))))
			
			} else {
				var result : Map[String, JsValue] = Map.empty
				val user = reVal.head
				List("signature", "role_tag", "screen_name", "screen_photo") foreach { x =>
					(data \ x).asOpt[String].map { value =>
					
					  	(data \ "isThird").asOpt[Int].map ( bt => Unit).getOrElse {
					  		user += x -> value
					  		result += x -> toJson(value)
					  	}
					}.getOrElse(Unit)
				}
				
				List("followings_count", "followers_count", "posts_count", "friends_count", "cycle_count", "isLogin") foreach { x => 
					(data \ x).asOpt[Int].map { value =>
						user += x -> new Integer(value)
						result += x -> toJson(value)
					}.getOrElse(Unit)
				}
			
				result += "user_id" -> toJson(user_id)
				_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
				Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(result)))
			}
		}
	}

	def queryUserProfile(user_id : String) : Map[String, JsValue] = {

		val re = from db() in "user_profile" where ("user_id" -> user_id) select (x => x) 
		if (re.empty) null 
		else {
			var tmp = Map.empty[String, JsValue]
			("user_id" :: "screen_name" :: "screen_photo" :: "role_tag" :: "signature" :: "followings_count" :: "followers_count" :: "posts_count" :: "friends_count" :: "cycle_count" :: "isLogin" :: Nil)
					.map(x => tmp += x -> helpOptions.opt_2_js(re.head.get(x), x))
			
			tmp
		}
	}
	
	def creatUserProfile(user_id : String, phoneNo : String) : Map[String, JsValue] = {
  	  	val builder = MongoDBObject.newBuilder
		builder += "user_id" -> user_id
		builder += "screen_name" -> user_id
		builder += "screen_photo" -> ""
		builder += "role_tag" -> ""
		builder += "followings_count" -> 0
		builder += "followers_count" -> 0
		builder += "friends_count" -> 0
		builder += "posts_count" -> 0
		builder += "cycle_count" -> 0
		builder += "isLogin" -> 0
		builder += "signature" -> ""

		val re = builder.result
		_data_connection.getCollection("user_profile") += re //builder.result
		
		var tmp = Map.empty[String, JsValue]
		("user_id" :: "screen_name" :: "screen_photo" :: "role_tag" :: "signature" :: "followings_count" :: "followers_count" :: "posts_count" :: "friends_count" :: "cycle_count" :: "isLogin" :: Nil)
//				.map(x => tmp += x -> helpOptions.opt_2_js(re.head.get(x), x))
				.map(x => tmp += x -> helpOptions.opt_2_js(Option(re.get(x)), x))
	
		println(tmp)
		tmp
	}
	
	/**
	 *  input: query_user_id, query_auth_token, owner_user_id
	 *  output: profile details
	 */
	def userProfile(data : JsValue) : JsValue = {
		
		val query_user_id = (data \ "query_user_id").asOpt[String].map(x => x).getOrElse("")
		val query_auth_token = (data \ "query_auth_token").asOpt[String].map(x => x).getOrElse("")
		val owner_user_id = (data \ "owner_user_id").asOpt[String].map(x => x).getOrElse("")
	 
		if (query_user_id == "" || query_auth_token == "") ErrorCode.errorToJson("token not valid")
		else if (owner_user_id == "")  ErrorCode.errorToJson("user not existing")
		else {
			// 1. TODO: check query_user_id and query_auth_token and is validate
			val re = from db() in "user_profile" where ("user_id" -> owner_user_id) select (x => x) 
			if (re.count != 1) ErrorCode.errorToJson("user not existing")
			// 2. 
			else {
				var tmp = Map.empty[String, JsValue]
				("user_id" :: "screen_name" :: "screen_photo" :: "role_tag" :: "signature" :: "followings_count" :: "followers_count" :: "posts_count" :: "friends_count" :: "cycle_count" :: "isLogin" :: Nil)
					.map(x => tmp += x -> helpOptions.opt_2_js(re.head.get(x), x))
					
				tmp += "relations" -> toJson(RelationshipModule.relationsBetweenUserAndPostowner(query_user_id, owner_user_id).con)
				Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
			}
		}
	}
	
	def updateFollowingCount(count : Int, user_id : String) = {
		val reVal = from db() in "user_profile" where ("user_id" -> user_id) select (x => x)
		if (!reVal.empty) {
			val user = reVal.head
			user += "followings_count" -> new Integer(count)
			_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
		}
	}
	
	def updateFollowedCount(count : Int, user_id : String) = {
		val reVal = from db() in "user_profile" where ("user_id" -> user_id) select (x => x)
		if (!reVal.empty) {
			val user = reVal.head
			user += "followers_count" -> new Integer(count)
			_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
		}  
	}
	
	def updateFriendsCount(count : Int, user_id : String) = {
		val reVal = from db() in "user_profile" where ("user_id" -> user_id) select (x => x)
		if (!reVal.empty) {
			val user = reVal.head
			user += "friends_count" -> new Integer(count)
			_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
		}  
	}
	
	/**
	 * mutiple user profile
	 */
	def multipleUserProfile(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val query_list = (data \ "query_list").asOpt[List[String]].get

		if (query_list.isEmpty) Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(query_list)))
		else {
			var conditions : DBObject = null
			query_list.foreach { x=> 
			  	if (conditions == null) {
			  		val builder = MongoDBObject.newBuilder  
			  		builder += "user_id" -> x
			  		conditions = builder.result
			  	}
				else conditions = $or(conditions, DBObject("user_id" -> x))
			}
			
			val reVal = from db() in "user_profile" where conditions select { x =>
				var tmp = Map.empty[String, JsValue]
				x.getAs[String]("user_id").map (id => tmp += "user_id" -> toJson(id)).getOrElse(throw new Exception("user not exists"))
				x.getAs[String]("screen_name").map (name => tmp += "screen_name" -> toJson(name)).getOrElse(tmp += "screen_name" -> toJson(""))
				x.getAs[String]("screen_photo").map (photo => tmp += "screen_photo" -> toJson(photo)).getOrElse(tmp += "screen_photo" -> toJson(""))
				x.getAs[String]("role_tag").map (tag => tmp += "role_tag" -> toJson(tag)).getOrElse(tmp += "role_tag" -> toJson(""))
				x.getAs[Int]("isLogin").map (tag => tmp += "isLogin" -> toJson(tag)).getOrElse(tmp += "isLogin" -> toJson(0))
				toJson(tmp)
			}
		
			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(reVal.toList.asInstanceOf[List[JsValue]])))		  
		}
	}
	
	def recommendUserProfile(data : JsValue) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(10)
		
		val reVal = (from db() in "user_profile").selectSkipTop(skip)(take)("user_id") { x =>
		  	var tmp = Map.empty[String, JsValue]
			x.getAs[String]("user_id").map (id => tmp += "user_id" -> toJson(id)).getOrElse(throw new Exception("user not exists"))
			x.getAs[String]("screen_name").map (name => tmp += "screen_name" -> toJson(name)).getOrElse(tmp += "screen_name" -> toJson(""))
			x.getAs[String]("screen_photo").map (photo => tmp += "screen_photo" -> toJson(photo)).getOrElse(tmp += "screen_photo" -> toJson(""))
			x.getAs[String]("role_tag").map (tag => tmp += "role_tag" -> toJson(tag)).getOrElse(tmp += "role_tag" -> toJson(""))
			x.getAs[Int]("isLogin").map (tag => tmp += "isLogin" -> toJson(tag)).getOrElse(tmp += "isLogin" -> toJson(0))
			toJson(tmp)
		}

		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(reVal.toList.asInstanceOf[List[JsValue]])))		  
	}
	
	/**
	 * increment cycle count
	 */
	def incrementCycleCount(user_id : String) = {
	  	val reVal = from db() in "user_profile" where ("user_id" -> user_id) select (x => x)
		if (!reVal.empty) {
			val user = reVal.head
			user.getAs[Int]("cycle_count").map(x => user += "cycle_count" -> new Integer(x + 1)).getOrElse(user += "cycle_count" -> new Integer(1))
			_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
		}  
	}
	
	/**
	 * decrement cycle count
	 */
	def decrementCycleCount(user_id : String) = {
	  	val reVal = from db() in "user_profile" where ("user_id" -> user_id) select (x => x)
		if (!reVal.empty) {
			val user = reVal.head
			user.getAs[Int]("cycle_count").map(x => user += "cycle_count" -> new Integer(x - 1)).getOrElse(user += "cycle_count" -> new Integer(0))
			_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
		} 
	}
}