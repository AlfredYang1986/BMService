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
import module.query.QueryModule

object PostModule {
	def postContent(data : JsValue) : JsValue = {
	 
		/**
		 * item list store in database
		 */
		def postItemList : MongoDBList = {
			val list_builder = MongoDBList.newBuilder
			(data \ "items").asOpt[Seq[JsValue]].get map { x =>
				
				val tmp = MongoDBObject.newBuilder
				tmp += "type" -> (x \ "type").asOpt[Int].get
				tmp += "name" -> (x \ "name").asOpt[String].get
				
				list_builder += tmp.result
			}
			
			list_builder.result
		}
		
		/**
		 * tags list store in database
		 * tags is not complusory
		 */
		def postTagsList : MongoDBList = {
			val list_builder = MongoDBList.newBuilder
			(data \ "tags").asOpt[Seq[JsValue]].map { iter => iter.map { x => 
				
				val tmp = MongoDBObject.newBuilder
				tmp += "type" -> (x \ "type").asOpt[Int].get
				tmp += "content" -> (x \ "content").asOpt[String].get
				tmp += "offsetX" -> (x \ "offsetX").asOpt[Double].map(x => x).getOrElse(-1.0)
				tmp += "offsetY" -> (x \ "offsetY").asOpt[Double].map(x => x).getOrElse(-1.0)
				
				list_builder += tmp.result
			}}.getOrElse(Unit)
			
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
		  
			val user_name = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""))).head
			val user_photo = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))).head
		  
			val builder = MongoDBObject.newBuilder
			builder += "post_id" -> Sercurity.md5Hash(user_id + user_name + Sercurity.getTimeSpanWithSeconds)
			builder += "date" -> new Date().getTime
			builder += "owner_id" -> user_id
			builder += "owner_name" -> user_name
			builder += "owner_photo" -> user_photo
			builder += "title" -> title
			builder += "description" -> description
			builder += "items" -> postItemList
			builder += "tags" -> postTagsList
			builder += "likes_count" -> 0
			builder += "likes" -> MongoDBList()
			builder += "comments_count" -> 0
			builder += "comments" -> MongoDBList()
			
			_data_connection.getCollection("posts") += builder.result
			
			/**
			 * refresh user profile table for resent
			 */
			val user_profile = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x)).head
			user_profile.get("posts_count").map { x => 
				  	val tmp : Number = x.asInstanceOf[Number].intValue + 1
				  	user_profile += "posts_count" -> tmp
				}.getOrElse(Unit)
			_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user_profile);
			
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("post_result" -> toJson(true)))))
		}
	}
	
	def uploadFile(data : MultipartFormData[TemporaryFile]) : JsValue = fop.uploadFile(data)
	
	def postCommnet(data : JsValue) : JsValue = {
	 
		def resentCommentCount = 2
	  
		def createComment(user_id : String, user_name : String, content : String, user_photo : String) : MongoDBObject = {
			val comment_builder = MongoDBObject.newBuilder
			comment_builder += "comment_owner_id" -> user_id
			comment_builder += "comment_owner_name" -> user_name
			comment_builder += "comment_owner_photo" -> user_photo
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
			val user_name = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""))).head
			val user_photo = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))).head
			
			/**
			 * add commment to comments table
			 */
			val ori_comments = from db() in "post_comments" where ("post_id" -> post_id) select (x => x)
			if (ori_comments.empty) {
				val comment = createComment(user_id, user_name, content, user_photo)
				
				val comment_list_builder = MongoDBList.newBuilder
				comment_list_builder += comment 
				
				val post_builder = MongoDBObject.newBuilder
				post_builder += "post_id" -> post_id
				post_builder += "comments" -> comment_list_builder.result//.sortBy{ x => x.asInstanceOf[BasicDBObject].get("comment_date").asInstanceOf[Number].longValue }.reverse
				
				_data_connection.getCollection("post_comments") += post_builder.result
			} else {
				val comment = createComment(user_id, user_name, content, user_photo)
			
				val ori = ori_comments.head
				ori.get("comments").map { x => 
				  	val xls = x.asInstanceOf[BasicDBList]
				  	xls.add(0, comment)
				}.getOrElse(Unit)
				
				_data_connection.getCollection("post_comments").update(DBObject("post_id" -> post_id), ori);
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
					val new_comments = (from db() in "post_comments" where ("post_id" -> post_id)).select(x => x.get("comments").get.asInstanceOf[DBObject])
					ori_post += "comments" -> new_comments.head.asInstanceOf[BasicDBList].take(resentCommentCount)
				}.getOrElse(Unit)

				_data_connection.getCollection("posts").update(DBObject("post_id" -> post_id), ori_post);
				QueryModule.queryComments(data)
			}
		}
	}
	
	def postLike(data : JsValue) : JsValue = {
	
		def resentLikedCount = 6
	  
		def createLike(user_id : String, user_name : String, user_photo : String) : MongoDBObject = {
			val like_builder = MongoDBObject.newBuilder
			like_builder += "like_owner_id" -> user_id
			like_builder += "like_owner_name" -> user_name
			like_builder += "like_owner_photo" -> user_photo
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
			val user_name = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""))).head
			val user_photo = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))).head
		  		
			/**
			 * add like to likes table
			 */
			val ori_likes = from db() in "post_likes" where ("post_id" -> post_id) select (x => x)
			if (ori_likes.empty) {
				val like = createLike(user_id, user_name, user_photo)
				
				val like_list_builder = MongoDBList.newBuilder
				like_list_builder += like
				
				val post_builder = MongoDBObject.newBuilder
				post_builder += "post_id" -> post_id
				post_builder += "likes" -> like_list_builder.result
			
				_data_connection.getCollection("post_likes") += post_builder.result
			} else {
				val like = createLike(user_id, user_name, user_photo)
			
				val ori = ori_likes.head
				ori.get("likes").map { x => 
				  	if (!x.asInstanceOf[BasicDBList].exists(iter => iter.asInstanceOf[DBObject].getAs[String]("like_owner_id").get.equals(user_id))) {
				  		x.asInstanceOf[BasicDBList].add(0, like)
				  		_data_connection.getCollection("post_likes").update(DBObject("post_id" -> post_id), ori);
				  	}
				  	else Unit
				}.getOrElse(Unit)
			}
		
			/**
			 * refresh user like post table
			 */
			val user_likes = from db() in "user_likes" where ("user_id" -> user_id) select (x => x)
			if (user_likes.empty) {
			  
				val ul = MongoDBObject.newBuilder
				ul += "user_id" -> user_id
			  
				val user_like_list = MongoDBList.newBuilder
				user_like_list += post_id
			
				ul += "likes" -> user_like_list.result
				_data_connection.getCollection("user_likes") += ul.result
			  
			} else {
			 
				val ul = user_likes.head
				ul.get("likes").map { x =>
					if (!x.asInstanceOf[BasicDBList].exists(iter => iter.asInstanceOf[String].equals(post_id))) {
						x.asInstanceOf[BasicDBList].add(0, post_id)
						_data_connection.getCollection("user_likes").update(DBObject("user_id" -> user_id), ul);
					}
					else Unit
				}.getOrElse(Unit)
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
					val new_likes = (from db() in "post_likes" where ("post_id" -> post_id)).select(x => x.get("likes").get.asInstanceOf[DBObject])
					ori_post += "likes" -> new_likes.head.asInstanceOf[BasicDBList].take(resentLikedCount)
				}.getOrElse(Unit)
				_data_connection.getCollection("posts").update(DBObject("post_id" -> post_id), ori_post);
				
				QueryModule.queryLikes(data)
			}
		}
	}
}