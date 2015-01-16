package module.post

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.login.LoginModule
import util.errorcode._
import java.util.Date
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile
import module.sercurity._
import module.common.files.fop

object PostModule {
	def postContent(data : JsValue) : JsValue = {
	 
		/**
		 * item list store in database
		 */
		def postItemList : MongoDBList = {
			val list_builder = MongoDBList.newBuilder
			(data \ "items").asOpt[Seq[JsValue]].get map { x =>
			  	val t = (x \ "type").asOpt[Int].get
				val url = (x \ "name").asOpt[String].get
				
				val tmp = MongoDBObject.newBuilder
				tmp += "type" -> t
				tmp += "name" -> url
			
				list_builder += tmp.result
			}
			
			list_builder.result
		}
		
		/**
		 * get data from token
		 */
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val description = (data \ "description").asOpt[String].get
		val location = (data \ "location").asOpt[String].map(x => x).getOrElse("")
		val title = (data \ "title").asOpt[String].map(x => x).getOrElse("")
		
		/**
		 * check the token is validate or not
		 */
		if (!LoginModule.isAuthTokenValidate(auth_token)) {
			ErrorCode.errorToJson("auth token not valid")
		} else if (!LoginModule.isUserExist(user_id)) {
			ErrorCode.errorToJson("unknown user")
		} else {
			/**
			 * save all the data to database
			 */
		  
			val user_name = (from db() in "users" where ("user_id" -> user_id) select (x => x.get("name").map(n=>n).getOrElse(user_id))).head.asInstanceOf[String]
		  
			val builder = MongoDBObject.newBuilder
			builder += "post_id" -> Sercurity.md5Hash(user_id + user_name + Sercurity.getTimeSpanWithSeconds)
			builder += "date" -> new Date().getTime
			builder += "owner_id" -> user_id
			builder += "owner_name" -> user_name
			builder += "title" -> title
			builder += "description" -> description
			builder += "items" -> postItemList
			builder += "likes_count" -> 0
			builder += "likes" -> MongoDBList()
			builder += "comments_count" -> 0
			builder += "comments" -> MongoDBList()
			
			_data_connection.getCollection("posts") += builder.result
			
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("post_result" -> toJson(true)))))
		}
	}
	
	def uploadFile(data : MultipartFormData[TemporaryFile]) : JsValue = fop.uploadFile(data)
	
	def postCommnet(data : JsValue) : JsValue = {
	  
		def createComment(user_id : String, user_name : String, content : String) : MongoDBObject = {
			val comment_builder = MongoDBObject.newBuilder
			comment_builder += "comment_owner_id" -> user_id
			comment_builder += "comment_owner_name" -> user_name
			comment_builder += "comment_owner_photo" -> ""
			comment_builder += "comment_date" -> new Date().getTime
			comment_builder += "comment_content" -> content
			
			comment_builder.result
		}
	  
		/**
		 * get arguments
		 */
		val post_id = (data \ "post_id").asOpt[String].map(x => x).getOrElse("")
		val user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse("")
		val auth_token = (data \ "auth_token").asOpt[String].map(x => x).getOrElse("")
		val content = (data \ "content").asOpt[String].map(x => x).getOrElse("")
		
		if (post_id == "" || user_id == "" || auth_token == "") ErrorCode.errorToJson("token not valid")
		else {
			val user_name = (from db() in "users" where ("user_id" -> user_id) select (x => x.get("name").map(n=>n).getOrElse(user_id))).head.asInstanceOf[String]
			
			/**
			 * add commment to comments table
			 */
			val ori_comments = from db() in "commments" where ("post_id" -> post_id) select (x => x)
			if (ori_comments.empty) {
				val comment = createComment(user_id, user_name, content)
				
				val comment_list_builder = MongoDBList.newBuilder
				comment_list_builder += comment 
				
				val post_builder = MongoDBList.newBuilder
				post_builder += "post_id" -> post_id
				post_builder += "comments" -> comment_list_builder.result
				
				_data_connection.getCollection("comments") += post_builder.result
			} else {
				val comment = createComment(user_id, user_name, content)
			
				val ori = ori_comments.head
				ori.get("comments").map(x => x.asInstanceOf[BasicDBList].add(comment)).getOrElse(Unit)
				
				_data_connection.getCollection("comments").update(DBObject("post_id" -> post_id), ori);
			}
			
			/**
			 * refresh main post table resent 
			 */
			val ori_posts = from db() in "posts" where ("post_id" -> post_id) select (x => x)
			if (ori_posts.empty) ErrorCode.errorToJson("post token not vaild")
			else {
				val ori_post = ori_posts.head
				ori_post.get("comments_count").map { x => 
				  	val tmp : Number = x.asInstanceOf[Number].intValue + 1
				  	ori_post += "comments_count" -> tmp
				}.getOrElse(Unit)
				
				ori_post.get("comments").map { x => 
					val ori_comment_list = x.asInstanceOf[BasicDBList]
					ori_comment_list += createComment(user_id, user_name, content)
					val new_comment_list = ori_comment_list.sortBy(x => x.asInstanceOf[BasicDBObject]
												.get("comment_date").asInstanceOf[Number].longValue).reverse.tail
												
					ori_post += "comments" -> new_comment_list
				}.getOrElse(Unit)
				_data_connection.getCollection("posts").update(DBObject("post_id" -> post_id), ori_post);
				
				Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("comment_result" -> toJson(true)))))
			}
		}
	}
	
	def likePost(data : JsValue) : JsValue = {
		
		def createLike(user_id : String, user_name : String) : MongoDBObject = {
			val like_builder = MongoDBObject.newBuilder
			like_builder += "like_owner_id" -> user_id
			like_builder += "like_owner_name" -> user_name
			like_builder += "like_owner_photo" -> ""
			like_builder += "like_date" -> new Date().getTime
			
			like_builder.result
		}
	  
		/**
		 * get arguments
		 */
		val post_id = (data \ "post_id").asOpt[String].map(x => x).getOrElse("")
		val user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse("")
		val auth_token = (data \ "auth_token").asOpt[String].map(x => x).getOrElse("")
	 
		if (post_id == "" || user_id == "" || auth_token == "") ErrorCode.errorToJson("token not valid")
		else {
			val user_name = (from db() in "users" where ("user_id" -> user_id) select (x => x.get("name").map(n=>n).getOrElse(user_id))).head.asInstanceOf[String]
		  		
			/**
			 * add like to likes table
			 */
			val ori_comments = from db() in "commments" where ("post_id" -> post_id) select (x => x)
			if (ori_comments.empty) {
				val comment = createLike(user_id, user_name)
				
				val comment_list_builder = MongoDBList.newBuilder
				comment_list_builder += comment 
				
				val post_builder = MongoDBList.newBuilder
				post_builder += "post_id" -> post_id
				post_builder += "likes" -> comment_list_builder.result
				
				_data_connection.getCollection("likes") += post_builder.result
			} else {
				val comment = createLike(user_id, user_name)
			
				val ori = ori_comments.head
				ori.get("likes").map(x => x.asInstanceOf[BasicDBList].add(comment)).getOrElse(Unit)
				
				_data_connection.getCollection("likes").update(DBObject("post_id" -> post_id), ori);
			}
			
			/**
			 * refresh main post table resent 
			 */
			val ori_posts = from db() in "posts" where ("post_id" -> post_id) select (x => x)
			if (ori_posts.empty) ErrorCode.errorToJson("post token not vaild")
			else {
				val ori_post = ori_posts.head
				ori_post.get("likes_count").map { x => 
				  	val tmp : Number = x.asInstanceOf[Number].intValue + 1
				  	ori_post += "likes_count" -> tmp
				}.getOrElse(Unit)
				
				ori_post.get("likes").map { x => 
					val ori_comment_list = x.asInstanceOf[BasicDBList]
					ori_comment_list += createLike(user_id, user_name)
												
					ori_post += "likes" -> ori_comment_list
				}.getOrElse(Unit)
				_data_connection.getCollection("posts").update(DBObject("post_id" -> post_id), ori_post);
				
				Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("comment_result" -> toJson(true)))))
			}
		}
	}
}