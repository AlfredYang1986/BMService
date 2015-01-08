package module.query

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

import module.common.files.fop

object QueryModule {
  
	/**
	 * for the initial stage, only can query yours data
	 */
  
	def queryHomeContent(data : JsValue) : JsValue = {
			
		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
	  
		null
	}
	
	def downloadFile(name : String) : Array[Byte] = fop.downloadFile(name)
}