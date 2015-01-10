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

import module.common.files.fop


object PostModule {
	def postContent(data : JsValue) : JsValue = {
	  
		/**
		 * get data from token
		 */
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val title = (data \ "title").asOpt[String].get
		val description = (data \ "description").asOpt[String].get
		
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
			builder += "date" -> new Date().getTime().toString 
			builder += "owner_id" -> user_id
			builder += "owner_name" -> user_name
			builder += "title" -> title
			builder += "description" -> description
			builder += "likes" -> 0
			
			val list_builder = MongoDBList.newBuilder
			val items = (data \ "items").asOpt[Seq[JsValue]].get map { x =>
			  	val t = (x \ "type").asOpt[Int].get
				val url = (x \ "name").asOpt[String].get
				
				val tmp = MongoDBObject.newBuilder
				tmp += "type" -> t
				tmp += "name" -> url
			
				list_builder += tmp.result
			}
			
			builder += "items" -> list_builder.result
			
			_data_connection.getCollection("posts") += builder.result
			
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("post_result" -> toJson(true)))))
		}
	}
	
	def uploadFile(data : MultipartFormData[TemporaryFile]) : JsValue = fop.uploadFile(data)
}