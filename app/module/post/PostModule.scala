package module.post

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import com.mongodb.casbah.commons.MongoDBObject
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.login.LoginModule
import util.errorcode._
import java.util.Date
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile
import java.io.File
import play.api.libs.Files
import java.io.FileInputStream

object PostModule {
	def postContent(data : JsValue) : JsValue = {
	  
		/**
		 * get data from token
		 */
		val auth_token = (data \ "auth_token").asOpt[String].get
		val message = (data \ "message").asOpt[String].get
		
		println(auth_token)
		println(message)
		
		/**
		 * check the token is validate or not
		 */
		if (!LoginModule.isAuthTokenValidate(auth_token)) {
			ErrorCode.errorToJson("auth token not valid")
		} else {
			/**
			 * save all the data to database
			 */
			val builder = MongoDBObject.newBuilder
			builder += "date" -> new Date().getTime().toString 
			builder += "owner" -> auth_token
			builder += "message" -> message
		
			val list_builder = MongoDBList.newBuilder
			val items = (data \ "items").asOpt[Seq[JsValue]].get map { x =>
				println(x)
			  	val t = (x \ "type").asOpt[String].get
				val url = (x \ "url").asOpt[String].get
				
				val tmp = MongoDBObject.newBuilder
				tmp += "type" -> t
				tmp += "url" -> url
			
				list_builder += tmp.result
			}
			
			builder += "items" -> list_builder.result
			
			_data_connection.getCollection("posts") += builder.result
			
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("post_result" -> toJson(true)))))
		}
	}
	
	def uploadFile(data : MultipartFormData[TemporaryFile]) : JsValue = {
	  	data.file("upload").map { x =>
	  	  	Files.moveFile(x.ref.file, new File("upload/" + x.filename), true, true)
	  
			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
	  	  	
	  	}.getOrElse {
			ErrorCode.errorToJson("post image error")
	  	} 
	}

	def downloadFile(name : String) : Array[Byte] = {
	  	val file = new File("upload/" + name)
		val reVal : Array[Byte] = new Array[Byte](file.length.intValue)
		new FileInputStream(file).read(reVal)
		reVal
	}
}