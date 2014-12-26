package module.login

import play.api.libs.json.JsValue
import util.dao.from
import com.mongodb.casbah.commons.MongoDBObject
import util.dao._data_connection

object LoginModule {
	def authWithPhone(data : JsValue) : JsValue = {
	
		val phoneNo = (data \ "phoneNo")
	
		/**
		 * generate code
		 */
		val code = 11111 // fake one
//		val code = Math.random % 10000.0

		/**
		 * TODO: send code to the phone
		 */
		
		/**
		 * generate a reg token
		 */
		
		val builder = MongoDBObject.newBuilder
		builder += "phoneNo" -> phoneNo
		builder += "code" -> code
			
		val rel = from db() in "reg" where ("phoneNo" -> phoneNo) select (x => x) 
		if (rel.empty) _data_connection.getCollection("reg") += builder.result
		else _data_connection.getCollection("reg").update(rel.head.asDBObject, builder.result)
	
		/**
		 * return 
		 */
		null
	}

	def authComfirm(data : JsValue) : JsValue = {
		null
	}
	
	def authWithThird(data : JsValue) : JsValue = {
		null
	}

	def connectWithThird(data : JsValue) : JsValue = {
		null
	}
}