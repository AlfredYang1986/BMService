package module.tags

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.common.helpOptions
import java.util.Date

object TagModule {
	def queryContentsWithTag(data : JsValue) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val tag_name = (data \ "tag_name").asOpt[String].get
		val tag_type = (data \ "tag_type").asOpt[Int].get
		
		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)
	
		// TODO: check the user_id is valid
		val user_check = from db() in "users" where ("user_id" -> user_id) select (x => x)
		if (user_check.count == 0) ErrorCode.errorToJson("user not existing")
		else {
			val result = from db() in "posts" where ("tags.content" -> tag_name, "tags.type" -> tag_type) select (x => x)
		
			var xls : List[JsValue] = Nil
			(from db() in "posts" where ("date" $lte date, "tags.content" -> tag_name, "tags.type" -> tag_type)).selectSkipTop(skip)(take)("date") { x => 
		  		var tmp : Map[String, JsValue] = Map.empty
		  		List("post_id", "date", "owner_id", "owner_name", "owner_photo", "location", "title", "description", "likes_count", "likes", "comments_count", "comments", "items", "tags") map (iter => tmp += iter -> helpOptions.opt_2_js(x.get(iter), iter))
		  		xls = xls :+ toJson(tmp)
			}

			Json.toJson(Map("status" -> toJson("ok"), "date" -> toJson(date), "result" -> toJson(xls)))
		}
	}
}