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
				
				_data_connection.getCollection("user_profile") += builder.result
			} else {
				val user = reVal.head
				List("role_tag", "screen_name", "screen_photo") foreach { x =>
					(data \ x).asOpt[String].map { value =>
						user += x -> value
					}.getOrElse(Unit)
				}
				
				List("followings_count", "followers_count", "posts_count", "friends_count", "cycle_count") foreach { x => 
					(data \ x).asOpt[Int].map { value =>
						user += x -> new Integer(value)
					}.getOrElse(Unit)
				}
		
				_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
			}
			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("user_id" -> user_id, "name" -> screen_name, "screen_photo" -> screen_photo, "role_tag" -> role_tag))))
		}
	}

	def queryUserProfile(user_id : String) : Map[String, JsValue] = {
		val re = from db() in "user_profile" where ("user_id" -> user_id) select (x => x) 
		if (re.count != 1) null
		else {
			var tmp = Map.empty[String, JsValue]
			("user_id" :: "screen_name" :: "screen_photo" :: "role_tag" :: "followings_count" :: "followers_count" :: "posts_count" :: "friends_count" :: "cycle_count" :: Nil)
					.map(x => tmp += x -> helpOptions.opt_2_js(re.head.get(x), x))
			
			tmp
		}
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
				("user_id" :: "screen_name" :: "screen_photo" :: "role_tag" :: "followings_count" :: "followers_count" :: "posts_count" :: "friends_count" :: "cycle_count" :: Nil)
					.map(x => tmp += x -> helpOptions.opt_2_js(re.head.get(x), x))
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
			toJson(tmp)
		}
	
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(reVal.toList.asInstanceOf[List[JsValue]])))
	}
}