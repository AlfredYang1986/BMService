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
import module.login.LoginModule
import module.query.QueryModule

import akka.actor.{Actor, Props}
import play.api.libs.concurrent.Akka
import play.api.GlobalSettings
import play.api.templates.Html
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import java.util.Date

object ProfileModule {

  /**
	 * update user profile, call by client
	 */
	def updateUserProfile(data : JsValue) : JsValue = {
		var user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse("")
		var auth_token = (data \ "auth_token").asOpt[String].map(x => x).getOrElse("")
		val screen_name = (data \ "screen_name").asOpt[String].map(x => x).getOrElse("")
		val screen_photo = (data \ "screen_photo").asOpt[String].map(x => x).getOrElse("")
		val role_tag = (data \ "role_tag").asOpt[String].map(x => x).getOrElse("")
		
		val createWhenNotExist = (data \ "create").asOpt[Int].map(x => x).getOrElse(0)
		val createNewAuthToken= (data \ "refresh_token").asOpt[Int].map(x => x).getOrElse(0)
 
    if (user_id == "") ErrorCode.errorToJson("user not existing")
		else {
			val reVal = from db() in "user_profile" where ("user_id" -> user_id) select (x => x)
			var result : Map[String, JsValue] = Map.empty
     
			(data \ "auth_token").asOpt[String].map(x => result += "auth_token" -> toJson(x)).getOrElse(Unit)
      (data \ "connect_result").asOpt[String].map(x => result += "connect_result" -> toJson(x)).getOrElse(Unit)

      if (createNewAuthToken != 0) {
            auth_token = LoginModule.refreshAuthToken(user_id, (data \ "uuid").asOpt[String].get)
            result += "auth_token" -> toJson(auth_token)
      }
      
			if (reVal.empty) { 
			   
        if (createWhenNotExist != 0) {
            val cn = LoginModule.authCreateUserWithPhone(data)
            user_id =  ((cn \ "result").asOpt[JsValue].get \ "user_id").asOpt[String].get
            auth_token =  ((cn \ "result").asOpt[JsValue].get \ "auth_token").asOpt[String].get
        }
			    
				val builder = MongoDBObject.newBuilder
				builder += "user_id" -> user_id // c_r_user_id
				builder += "screen_name" -> screen_name
				builder += "screen_photo" -> screen_photo
				builder += "role_tag" -> role_tag

				builder += "followings_count" -> (data \ "followings_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "followers_count" -> (data \ "followers_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "friends_count" -> (data \ "friends_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "posts_count" -> (data \ "posts_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "cycle_count" -> (data \ "cycle_count").asOpt[Int].map(x => x).getOrElse(0)
				builder += "isLogin" -> (data \ "isLogin").asOpt[Int].map(x => x).getOrElse(1)
				builder += "gender" -> (data \ "gender").asOpt[Int].map(x => x).getOrElse(0)
				builder += "signature" -> (data \ "signature").asOpt[String].map(x => x).getOrElse("")

				val coordinate = MongoDBObject.newBuilder
				coordinate += "longtitude" -> (data \ "longtitude").asOpt[Float].map(x => x).getOrElse(0.toFloat.asInstanceOf[Number])
				coordinate += "latitude" -> (data \ "latitude").asOpt[Float].map(x => x).getOrElse(0.toFloat.asInstanceOf[Number])
				builder += "coordinate" -> coordinate.result
				
				builder += "address" -> (data \ "address").asOpt[String].map (x => x).getOrElse("")
				builder += "date" -> new Date().getTime
				builder += "dob" -> (data \ "dob").asOpt[Long].map (x => x).getOrElse(0.toFloat.asInstanceOf[Number])
				builder += "about" -> (data \ "about").asOpt[String].map (x => x).getOrElse("")
				
				(data \ "kids").asOpt[List[JsValue]].map { lst => 
				    val kids = MongoDBList.newBuilder
				    lst foreach { tmp => 
				        val kid = MongoDBObject.newBuilder
				        kid += "gender" -> (tmp \ "gender").asOpt[Int].map (x => x).getOrElse(0)
				        kid += "dob" -> (tmp \ "dob").asOpt[Long].map (x => x).getOrElse(0.toFloat.asInstanceOf[Number])

				        kids += kid.result
				    }
				    builder += "kids" -> kids.result
				}.getOrElse(builder += "kids" -> MongoDBList.newBuilder.result)
				
				result += "user_id" -> toJson(user_id) //toJson(c_r_user_id)
				result += "auth_token" -> toJson(auth_token) //toJson(c_r_user_id)
				result += "screen_name" -> toJson(screen_name)
				result += "screen_photo" -> toJson(screen_photo)
				result += "role_tag" -> toJson(role_tag)
        RoleTagModule.addRoleTags(toJson(Map("tag_name" -> role_tag)))    
			
				_data_connection.getCollection("user_profile") += builder.result
				Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(result)))
			
			} else {
				val user = reVal.head
				List("signature", "role_tag", "screen_name", "screen_photo", "about", "address") foreach { x =>
					(data \ x).asOpt[String].map { value =>
					
//					  	(data \ "isThird").asOpt[Int].map ( bt => Unit).getOrElse {
					  		user += x -> value
					  		result += x -> toJson(value)
					  		if (x == "role_tag") {
					  		    RoleTagModule.addRoleTags(toJson(Map("tag_name" -> role_tag)))    
					  		}
//					    }
					}.getOrElse(Unit)
				}
				
				List("followings_count", "followers_count", "posts_count", "friends_count", "cycle_count", "isLogin", "gender") foreach { x => 
					(data \ x).asOpt[Int].map { value =>
						user += x -> new Integer(value)
						result += x -> toJson(value)
					}.getOrElse(Unit)
				}

				List("dob") foreach { x => 
					(data \ x).asOpt[Long].map { value =>
						user += x -> value.asInstanceOf[Number]
						result += x -> toJson(value)
					}.getOrElse(Unit)
				}
			
				List("longtitude", "latitude") foreach { x => 
					(data \ x).asOpt[Float].map { value =>
						val co = user.getAs[MongoDBObject]("coordinate").get
						co += x -> x.asInstanceOf[Number]
						result += x -> toJson(value)
					}.getOrElse(Unit)
				}
				
				(data \ "kids").asOpt[List[JsValue]].map { lst => 
				    val kids = MongoDBList.newBuilder
				    lst foreach { tmp => 
				        val kid = MongoDBObject.newBuilder
				        kid += "gender" -> (tmp \ "gender").asOpt[Int].map (x => x).getOrElse(0)
				        kid += "dob" -> (tmp \ "dob").asOpt[Long].map (x => x).getOrElse(0.toFloat.asInstanceOf[Number])

				        kids += kid.result
				    }
				    user += "kids" -> kids.result
				}.getOrElse(Unit)
		
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
			("user_id" :: "screen_name" :: "screen_photo" :: "role_tag" :: "signature" 
			        :: "followings_count" :: "followers_count" :: "posts_count" :: "friends_count" 
			        :: "cycle_count" :: "isLogin" :: "gender" :: "been_pushed" :: "been_liked" :: Nil)
					.foreach { x => 
					    tmp += x -> helpOptions.opt_2_js_2(re.head.get(x), x)(str =>
                  if (str.equals("gender")) toJson(0)
                  else toJson(""))}
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
  		builder += "gender" -> 0
  		builder += "signature" -> ""
  		
			val coordinate = MongoDBObject.newBuilder
			coordinate += "longtitude" -> 0.toFloat.asInstanceOf[Number]
			coordinate += "latitude" -> 0.toFloat.asInstanceOf[Number]
			builder += "coordinate" -> coordinate.result
				
			builder += "address" -> ""
			builder += "date" -> new Date().getTime
			builder += "dob" -> 0.toFloat.asInstanceOf[Number]
			builder += "about" -> ""
				
			builder += "kids" -> MongoDBList.newBuilder.result

  		val re = builder.result
  		_data_connection.getCollection("user_profile") += re //builder.result
  		
  		var tmp = Map.empty[String, JsValue]
  		("user_id" :: "screen_name" :: "screen_photo" :: "role_tag" :: "signature" 
  		        :: "followings_count" :: "followers_count" :: "posts_count" :: "friends_count" 
  		        :: "cycle_count" :: "isLogin" :: "gender" :: Nil)
  				.foreach { x => 
  				    tmp += x -> helpOptions.opt_2_js_2(Option(re.get(x)), x)(str =>
                    if (str.equals("gender")) toJson(0)
                    else toJson(""))}
  		tmp
	}
	
	/**
	 *  input: query_user_id, query_auth_token, owner_user_id
	 *  output: profile details
	 */
	def userProfile(data : JsValue)(cur : MongoDBObject) : JsValue = {
	   
		val query_user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse("")
		val query_auth_token = (data \ "auth_token").asOpt[String].map(x => x).getOrElse("")
		val owner_user_id = (data \ "owner_user_id").asOpt[String].map(x => x).getOrElse("")
	 
			// 1. TODO: check query_user_id and query_auth_token and is validate
			val re = from db() in "user_profile" where ("user_id" -> owner_user_id) select (x => x) 
			if (re.count != 1) ErrorCode.errorToJson("user not existing")
			// 2. 
			else {
				var tmp = Map.empty[String, JsValue]
				("user_id" :: "screen_name" :: "screen_photo" :: "role_tag" :: "signature" 
				        :: "followings_count" :: "followers_count" :: "posts_count" :: "friends_count" 
				        :: "cycle_count" :: "isLogin" :: "gender" :: "been_pushed" :: "been_liked" :: Nil)
					.foreach { x => tmp += x -> helpOptions.opt_2_js_2(re.head.get(x), x)(str =>
                  if (str.equals("gender")) toJson(0)
                  else toJson(""))}
					
				tmp += "relations" -> toJson(RelationshipModule.relationsBetweenUserAndPostowner(query_user_id, owner_user_id).con)
			  tmp += "likes_count" -> toJson(QueryModule.queryUserLikesCount(query_user_id).intValue)
				
				Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
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
//	def multipleUserProfile(data : JsValue) : JsValue = {
	def multipleUserProfile(data : JsValue)(cur : MongoDBObject) : JsValue = {
		
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
				val id = x.getAs[String]("user_id").get
				x.getAs[String]("user_id").map { id => 
				    tmp += "user_id" -> toJson(id)
    				tmp += "relations" -> toJson(RelationshipModule.relationsBetweenUserAndPostowner(user_id, id).con)
				}.getOrElse(throw new Exception("user not exists"))
				x.getAs[String]("screen_name").map (name => tmp += "screen_name" -> toJson(name)).getOrElse(tmp += "screen_name" -> toJson(""))
				x.getAs[String]("screen_photo").map (photo => tmp += "screen_photo" -> toJson(photo)).getOrElse(tmp += "screen_photo" -> toJson(""))
				x.getAs[String]("role_tag").map (tag => tmp += "role_tag" -> toJson(tag)).getOrElse(tmp += "role_tag" -> toJson(""))
				x.getAs[Int]("isLogin").map (tag => tmp += "isLogin" -> toJson(tag)).getOrElse(tmp += "isLogin" -> toJson(0))
				toJson(tmp)
			}
		
			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(reVal.toList.asInstanceOf[List[JsValue]])))		  
		}
	}
	
//	def recommendUserProfile(data : JsValue) : JsValue = {
	def recommendUserProfile(data : JsValue)(cur : MongoDBObject) : JsValue = {
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