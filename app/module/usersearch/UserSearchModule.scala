package module.usersearch

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
//import module.common.helpOptions
import java.util.Date
import module.common.helpOptions
import module.relationship._

object UserSearchModule {
	def queryRecommandUsers(data : JsValue) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		
		(from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
		  case head :: Nil =>
			  Json.toJson(Map("status" -> toJson("ok"), "recommandUsers" -> toJson(
			  (from db() in "user_profile").selectSkipTop(skip)(take)("user_id") { x => 
			  	  val id = x.getAs[String]("user_id").get
//			  	  val post_preview = (from db() in "posts" where ("owner_id" -> id)).selectSkipTop(skip)(take)("date"){ y => 
			  	  val post_preview = (from db() in "posts" where ("owner_id" -> id)).selectTop(3)("date"){ y => 
			  	      toJson(Map("post_id" -> toJson(y.getAs[String]("post_id").get), 
			  	    		  "items" -> toJson(((y.getAs[MongoDBList]("items").get.toSeq) map { y => y match {
			  	    				  	case item : BasicDBObject=>
			  	    				  		toJson(Map("name" -> toJson(item.get("name").asInstanceOf[String]), "type" -> toJson(item.get("type").asInstanceOf[Number].intValue)))
			  	    				  	case _ => ???
			  	    				  }}).toList)))
			  	  		}.toList
			  	  		
			  	  	  toJson(Map("user_id" -> toJson(id),
			  	  			   "role_tag" -> toJson(x.getAs[String]("role_tag").get),
				  			     "screen_name" -> toJson(x.getAs[String]("screen_name").get), 
				  			     "screen_photo" -> toJson(x.getAs[String]("screen_photo").get),
				  			     "relations" -> toJson(RelationshipModule.relationsBetweenUserAndPostowner(user_id, id).con),
				  			     "preview" -> toJson(post_preview)))
			  }.toList )))
		  case _ => null
		}
	}

	def queryUsersWithRoleTag(data : JsValue) : JsValue = ???
	
	def queryUsersPosts(data : JsValue) : JsValue = {
	    
	    val user_id = (data \ "user_id").asOpt[String].get
	    val auth_token = (data \ "auth_token").asOpt[String].get
	    val owner_id = (data \ "owner_id").asOpt[String].get
	    
	    val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
	    val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20)
	    val date = (data \ "data").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
	   
	    (from db() in "users" where ("user_id" -> user_id) select (x => x.getAs[String]("user_id").get)).toList match {
	        case Nil => ErrorCode.errorToJson("user not existing")
	        case head :: Nil => { 
			        Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(
	              (from db() in "posts" where ("owner_id" -> owner_id)).selectSkipTop(skip)(take)("date"){ x => 
            		  	var tmp : Map[String, JsValue] = Map.empty
            		  	List("post_id", "date", "owner_id", "owner_name", "owner_photo", "location", "title", "description", "likes_count", "likes", "comments_count", "comments", "items", "tags")
            		  	  .map (iter => tmp += iter -> helpOptions.opt_2_js(x.get(iter), iter))
            		  	  
            		  	val con = RelationshipModule.relationsBetweenUserAndPostowner(user_id, owner_id)
            		  	tmp += "relations" -> toJson(con.con)
            		  	tmp
	              }.toList)))
	        }
	        case _ => ???
	    }
	}
	
	def queryUsersWithScreenName(data : JsValue) : JsValue = {
	    
	    val user_id = (data \ "user_id").asOpt[String].get
	    val auth_token = (data \ "auth_token").asOpt[String].get
	    val screen_name = (data \ "screen_name").asOpt[String].get
	    
	    val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
	    val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20)
	   
	    (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
	        case Nil => ErrorCode.errorToJson("user not existing")
	        case head :: Nil => {
			      Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(
	            (from db() in "user_profile" where ("screen_name" $regex ("(?i)" + screen_name))).selectSkipTop(skip)(take)("date"){ x => 
	               val id = x.getAs[String]("user_id").get
	               toJson(Map("user_id" -> toJson(id),
	                         "screen_name" -> toJson(x.getAs[String]("screen_name").get),
	                         "screen_photo" -> toJson(x.getAs[String]("screen_photo").get),
	                         "role_tag" -> toJson(x.getAs[String]("role_tag").get),
	                         "relations" -> toJson(RelationshipModule.relationsBetweenUserAndPostowner(user_id, id).con)))
	            }.toList)))
	        }
	        case _ => ???
	    }
	}
	
	def queryRecommandUsersWithRoleTag(data : JsValue) : JsValue = {
	  val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val role_tag = (data \ "role_tag").asOpt[String].get
		
//		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(20)
//		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		
		(from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
		  case head :: Nil =>
			  Json.toJson(Map("status" -> toJson("ok"), "recommandUsers" -> toJson(
			  (from db() in "user_profile" where ("role_tag" -> role_tag)).select { x => 
			  	  val id = x.getAs[String]("user_id").get
//			  	  val post_preview = (from db() in "posts" where ("owner_id" -> id)).select { y => 
			  	  val post_preview = (from db() in "posts" where ("owner_id" -> id)).selectTop(3)("date"){ y => 
			  	      toJson(Map("post_id" -> toJson(y.getAs[String]("post_id").get), 
			  	    		  "items" -> toJson(((y.getAs[MongoDBList]("items").get.toSeq) map { y => y match {
			  	    				  	case item : BasicDBObject=>
			  	    				  		toJson(Map("name" -> toJson(item.get("name").asInstanceOf[String]), "type" -> toJson(item.get("type").asInstanceOf[Number].intValue)))
			  	    				  	case _ => ???
			  	    				  }}).toList)))
			  	  		}.toList
			  	  		
			  	  	  toJson(Map("user_id" -> toJson(id),
			  	  			   "role_tag" -> toJson(x.getAs[String]("role_tag").get),
				  			     "screen_name" -> toJson(x.getAs[String]("screen_name").get), 
				  			     "screen_photo" -> toJson(x.getAs[String]("screen_photo").get),
				  			     "relations" -> toJson(RelationshipModule.relationsBetweenUserAndPostowner(user_id, id).con),
				  			     "preview" -> toJson(post_preview)))
			  }.toList )))
		  case _ => null
		}
	}
	
	def queryUserScreenWithId(data : JsValue) : JsValue = {
	    
	    val user_id = (data \ "user_id").asOpt[String].get
	    val auth_token = (data \ "auth_token").asOpt[String].get
	    val query_id = (data \ "query_id").asOpt[String].get
	    
	    (from db() in "user_profile" where("user_id" -> query_id) select (x => x)).toList match {
	        case Nil => ErrorCode.errorToJson("user not existing")
	        case head :: Nil => {
			        Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(
			            head.getAs[String]("screen_name").get         
			        )))
	        }
	        case _ => ???
	    }
	}
}