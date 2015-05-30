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
import scala.collection.JavaConversions._

object QueryModule {
  
	def opt_2_js(value : Option[AnyRef], key : String) : JsValue = { 
		def opt_str_2_js(value : String) : JsValue = toJson(value)
		def opt_val_2_js(value : Number) : JsValue = toJson(value.longValue())
		def opt_map_2_js(value : BasicDBList, key : String) : JsValue = {
			def key2List : List[String] = key match {
				case "items" => List("type", "name")
				case "comments" => List("comment_owner_id", "comment_owner_name", "comment_date", "comment_content", "comment_owner_photo")
				case "likes" => List("like_owner_id", "like_owner_name", "like_owner_photo", "like_date")
			}
			var xls : List[JsValue] = Nil
			value.map { x =>
				var tmp : Map[String, JsValue] = Map.empty
				key2List map { iter =>
					x.asInstanceOf[BasicDBObject].get(iter) match {
				  		case str : String => tmp += iter -> opt_str_2_js(str)
				  		case list : BasicDBList => tmp += iter -> opt_map_2_js(list, key)
				  		case n : Number => tmp += iter -> opt_val_2_js(n)
				  		case _ => Unit
				  	}
				}			  
				xls = xls :+ toJson(tmp)
			}
			Json.toJson(xls)
		}
	  
		value.map ( x => x match {
			case str : String => opt_str_2_js(str)
		  	case list : BasicDBList => opt_map_2_js(list, key)
		  	case n : Number => opt_val_2_js(n)
		  	case _ => ??? 
		}).getOrElse(toJson(""))
	}
  
	/**
	 * for the initial stage, only can query yours data
	 */
	def queryHomeContent(data : JsValue) : JsValue = {

		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
		
		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)

		var xls : List[JsValue] = Nil
		(from db() in "posts" where ("date" $lte date)).selectSkipTop(skip)(take)("date") { x => 
		  	var tmp : Map[String, JsValue] = Map.empty
		  	List("post_id", "date", "owner_id", "owner_name", "owner_photo", "location", "title", "description", "likes_count", "likes", "comments_count", "comments", "items") map (iter => tmp += iter -> opt_2_js(x.get(iter), iter))
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
				List("comment_owner_id", "comment_owner_name", "comment_owner_photo", "comment_date", "comment_content") map (iter => tmp += iter -> opt_2_js(Option(x.asInstanceOf[BasicDBObject].get(iter)), iter))
				xls = xls :+ toJson(tmp)
			}

		Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(Map("comments_count" -> toJson(size), "comments" -> toJson(xls)))))
	}
	
	def queryLikes(data : JsValue) : JsValue = {
	
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
				List("like_owner_id", "like_owner_name", "like_owner_photo", "like_date") map (iter => tmp += iter -> opt_2_js(Option(x.asInstanceOf[BasicDBObject].get(iter)), iter))
				xls = xls :+ toJson(tmp)
			}

		Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(Map("likes_count" -> toJson(size), "likes" -> toJson(xls)))))
	}
}