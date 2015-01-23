package module.profile

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object ProfileModule {
	def like(data : JsValue) : JsValue = {
		null
	}
	
	/**
	 *  input: query_user_id, query_auth_token, owner_user_id
	 *  output: profile details
	 */
	def userProfile(data : JsValue) : JsValue = {
		
		val query_user_id = (data \ "query_user_id").asOpt[String].map(x => x).getOrElse("")
		val query_auth_token = (data \ "query_auth_token").asOpt[String].map(x => x).getOrElse("")
		val owner_user_id = (data \ "owner_user_id").asOpt[String].map(x => x).getOrElse("")
	 
		if (query_user_id == "" || query_auth_token == "") ErrorCode.errorToJson("token not valid")
		else if (owner_user_id == "")  ErrorCode.errorToJson("user not existing")
		else {
			// 1. TODO: check query_user_id and query_auth_token and is validate
			
			// 2. 
		}
		
		null
	}
}