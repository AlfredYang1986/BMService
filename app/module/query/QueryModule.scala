package module.query

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import java.util.Date
import module.common.files.fop
import module.common.helpOptions
import module.relationship.RelationshipModule
import scala.collection.JavaConversions._
import module.groups.GroupModule2
import module.post.PostModule
import module.profile.ProfileModule

object QueryModule {
	/**
	 * for the initial stage, only can query yours data
	 */
//	def queryHomeContent(data : JsValue) : JsValue = {
	def queryHomeContent(data : JsValue)(cur : MongoDBObject) : JsValue = {

		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
		
		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)

		var xls : List[JsValue] = Nil
		(from db() in "posts" where ("date" $lte date)).selectSkipTop(skip)(take)("date") { x => 
		  	var tmp : Map[String, JsValue] = Map.empty
		  	List("post_id", "date", "owner_id", "owner_name", "owner_photo", "location", "title", "description", "likes_count", "likes", "comments_count", "comments", "items", "tags") map (iter => tmp += iter -> helpOptions.opt_2_js(x.get(iter), iter))
		  	
		  	val con = RelationshipModule.relationsBetweenUserAndPostowner(user_id, tmp.get("owner_id").get.asOpt[String].get)
		  	tmp += "relations" -> toJson(con.con)
		  
		  	val post_id = x.getAs[String]("post_id").get
		  	val (photo_list, group_chat_count) = GroupModule2.queryPhotoListAndCount(post_id)
	  	  tmp += "group_chat_count" -> toJson(group_chat_count)
	  	  tmp += "group_user_list" -> toJson(photo_list)
		  
	  	  val isThumbup = PostModule.isLiked(user_id, post_id)
	  	  tmp += "isLiked" -> toJson(isThumbup)

	  	  val isPush = PostModule.isPush(user_id, post_id)
	  	  tmp += "isPush" -> toJson(isPush)
	  	  
	  	  val role_tag = ProfileModule.queryUserProfile(user_id).get("role_tag").get
	  	  tmp += "role_tag" -> toJson(role_tag)
	  	  
		  	xls = xls :+ toJson(tmp)
		}

		Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(xls)))
	}
	
//	def queryContentWithConditions(data : JsValue) : JsValue = {
	def queryContentWithConditions(data : JsValue)(cur : MongoDBObject) : JsValue = {
	  
		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
		
		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)	
		
		val conditions = (data \ "conditions").asOpt[JsValue].map (x => x).getOrElse(null)

		def dateConditions : DBObject = "date" $lte date
		  
		def constructConditions : DBObject = {
			val reVal = dateConditions
			
			if (conditions != null) {
			  	List("owner_id") foreach { x => 
			  		(conditions \ x).asOpt[String].map { cdt =>
			  			reVal += (x -> cdt)
			  		}
			  	}
			}
			reVal
		}

		var xls : List[JsValue] = Nil
		(from db() in "posts" where constructConditions).selectSkipTop(skip)(take)("date") { x => 
			var tmp : Map[String, JsValue] = Map.empty
		  	List("post_id", "date", "owner_id", "owner_name", "owner_photo", "location", "title", "description", "likes_count", "likes", "comments_count", "comments", "items", "tags") map (iter => tmp += iter -> helpOptions.opt_2_js(x.get(iter), iter))
		  	/**
		  	 * add post owner relations to the query user
		  	 */
		  	val con = RelationshipModule.relationsBetweenUserAndPostowner(user_id, tmp.get("owner_id").get.asOpt[String].get)
		  	tmp += "relations" -> toJson(con.con)		  	
		 
		  	val post_id = x.getAs[String]("post_id").get
		  	val (photo_list, group_chat_count) = GroupModule2.queryPhotoListAndCount(post_id)
	  	  tmp += "group_chat_count" -> toJson(group_chat_count)
	  	  tmp += "group_user_list" -> toJson(photo_list)

	  	  val isThumbup = PostModule.isLiked(user_id, post_id)
	  	  tmp += "isLiked" -> toJson(isThumbup)

	  	  val isPush = PostModule.isPush(user_id, post_id)
	  	  tmp += "isPush" -> toJson(isPush)
	  	 
	  	  tmp += "role_tag" -> toJson((ProfileModule.queryUserProfile(tmp.get("owner_id").get.asOpt[String].get)).get("role_tag").get.asOpt[String].get)

		  	xls = xls :+ toJson(tmp)
		}	

		Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(xls))) 
	}
	
	def downloadFile(name : String) : Array[Byte] = fop.downloadFile(name)
	
	def queryComments(data : JsValue) : JsValue = {
		
		val post_id = (data \ "post_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
		
		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)

		val comments_ori = (from db() in "post_comments" where ("post_id" -> post_id)).select(x => x.get("comments").get.asInstanceOf[DBObject])
		val comments = comments_ori.head.asInstanceOf[BasicDBList].filter(x => x.asInstanceOf[BasicDBObject].get("comment_date").asInstanceOf[Number].longValue < date)
		var xls : List[JsValue] = Nil
		
		val size = comments.size
		if (skip < size) 
			comments.subList(skip, Math.min(size, skip + take)).toSeq.map { x => 
				var tmp : Map[String, JsValue] = Map.empty
				List("comment_owner_id", "comment_owner_name", "comment_owner_photo", "comment_date", "comment_content") map (iter => tmp += iter -> helpOptions.opt_2_js(Option(x.asInstanceOf[BasicDBObject].get(iter)), iter))
				xls = xls :+ toJson(tmp)
			}

		Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(Map("comments_count" -> toJson(size), "comments" -> toJson(xls)))))
	}

	def queryPush(data : JsValue) : JsValue = {
	    val post_id = (data \ "post_id").asOpt[String].get
  		val auth_token = (data \ "auth_token").asOpt[String].get
		  val user_id = (data \ "user_id").asOpt[String].get
		
		  val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		  val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		  val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)
	   
		  val likes = (from db() in "post_push" where ("post_id" -> post_id)).select(x => x.get("push").get.asInstanceOf[DBObject])
  		var xls : List[JsValue] = Nil
  		val size = likes.head.size
  		if (skip < size) 
  			likes.head.asInstanceOf[BasicDBList].filter(x => x.asInstanceOf[BasicDBObject].get("push_date").asInstanceOf[Number].longValue < date).subList(skip, Math.min(size, skip + take)).toSeq.map { x => 
  				var tmp : Map[String, JsValue] = Map.empty
  				List("push_owner_id", "push_owner_name", "push_owner_photo", "push_date") map (iter => tmp += iter -> helpOptions.opt_2_js(Option(x.asInstanceOf[BasicDBObject].get(iter)), iter))
  				xls = xls :+ toJson(tmp)
  			}
  
  		Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(Map("push_count" -> toJson(size), "push" -> toJson(xls)))))
	}
	
//	def queryUserPush(data : JsValue) : JsValue = {
	def queryUserPush(data : JsValue)(cur : MongoDBObject) : JsValue = {
	    
	    val user_id = (data \ "user_id").asOpt[String].get
	    val auth_token = (data \ "auth_token").asOpt[String].get
	    
      val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		  val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		  val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(20)
		 
		  var conditions : DBObject = null
		  (from db() in "user_push" where ("user_id" -> user_id)).selectSkipTop(skip)(take)("data") { x => 
		      x.getAs[MongoDBList]("push").map { lst =>
		           lst.toList foreach { iter =>
		                 val post_id = iter.asInstanceOf[String]
		                 println(post_id)
		                 if (conditions == null) conditions = "post_id" $eq post_id
		                 else conditions = $or("post_id" $eq post_id, conditions)
		           }
		      }.getOrElse(Unit)
	    }
	   
		  var xls : List[JsValue] = Nil
	    if (conditions != null) {
	       	(from db() in "posts" where conditions).select { x => 
    		  	  var tmp : Map[String, JsValue] = Map.empty
    		  	  List("post_id", "date", "owner_id", "owner_name", "owner_photo", "location", "title", "description", "likes_count", "likes", "comments_count", "comments", "items", "tags") map (iter => tmp += iter -> helpOptions.opt_2_js(x.get(iter), iter))
    		  	
    		  	  val con = RelationshipModule.relationsBetweenUserAndPostowner(user_id, tmp.get("owner_id").get.asOpt[String].get)
    		  	  tmp += "relations" -> toJson(con.con)
    		  	  xls = xls :+ toJson(tmp)
    		  } 
	    }

	    Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(xls)))
	}

	def queryUserLikesCount(user_id : String) : Integer =
	    (from db() in "user_likes" where ("user_id" -> user_id) select (x => x.getAs[MongoDBList]("likes").get)) toList match {
	        case head :: Nil => head.count (_ => true)
	        case _ => 0
	    }
	
//	def queryLikes(data : JsValue) : JsValue = {
	def queryLikes(data : JsValue)(cur : MongoDBObject) : JsValue = {
	
		val post_id = (data \ "post_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
		
		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)
	
		val likes = (from db() in "post_likes" where ("post_id" -> post_id)).select(x => x.get("likes").get.asInstanceOf[DBObject])
		var xls : List[JsValue] = Nil
		val size = likes.head.size
		if (skip < size) 
			likes.head.asInstanceOf[BasicDBList].filter(x => x.asInstanceOf[BasicDBObject].get("like_date").asInstanceOf[Number].longValue < date).subList(skip, Math.min(size, skip + take)).toSeq.map { x => 
				var tmp : Map[String, JsValue] = Map.empty
				List("like_owner_id", "like_owner_name", "like_owner_photo", "like_date") map (iter => tmp += iter -> helpOptions.opt_2_js(Option(x.asInstanceOf[BasicDBObject].get(iter)), iter))
				xls = xls :+ toJson(tmp)
			}

		Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(Map("likes_count" -> toJson(size), "likes" -> toJson(xls)))))
	}
}