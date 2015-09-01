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

object CollectionQueryModule {

	def queryCollectionContent(data : JsValue) : JsValue = {
	  
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		val owner_id = (data \ "owner_id").asOpt[String].get
	
		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(20)
		
		val post_ids = (from db() in "user_likes" where ("user_id" -> owner_id) select (x => x.getAs[MongoDBList]("likes").get)).head.toList
//			x.getAs[MongoDBList]("likes").map { lst => 
//				lst.foreach { post_id => 
//					val tmp = MongoDBObject.newBuilder
//					tmp = "post_id" -> 
//				}
//			}
//		}.head

		var xls : List[JsValue] = Nil
		if (post_ids != null) {
		
			var conditions : DBObject = null
			
			post_ids.splitAt(skip)._2.take(take).foreach { x => 
			  	if (conditions == null) conditions = "post_id" $eq x.asInstanceOf[String]
			  	else  conditions = $or(conditions, "post_id" $eq x.asInstanceOf[String]) 
			}
			
			(from db() in "posts" where (conditions.asInstanceOf[DBObject])).selectSkipTop(skip)(take)("date") { x => 
		  		var tmp : Map[String, JsValue] = Map.empty
		  		List("post_id", "date", "owner_id", "owner_name", "owner_photo", "location", "title", "description", "likes_count", "likes", "comments_count", "comments", "items", "tags") map (iter => tmp += iter -> helpOptions.opt_2_js(x.get(iter), iter))
		  	
		  		val con = RelationshipModule.relationsBetweenUserAndPostowner(user_id, tmp.get("owner_id").get.asOpt[String].get)
		  		tmp += "relations" -> toJson(con.con)
		  		xls = xls :+ toJson(tmp)
			}
		}

		Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(xls)))
	}
}