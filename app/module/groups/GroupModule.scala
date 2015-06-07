package module.groups

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import java.util.Date
import module.common.helpOptions

object GroupModule {
	def createSubGroup(data : JsValue) : JsValue = {
		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
	
		if (!canUserCreateChatGroup(user_id)) ErrorCode.errorToJson("user have low authrity")
		
		val group_id = (data \ "group_id").asOpt[String].get
		val sub_group_id = (data \ "sub_group_id").asOpt[String].map(x => x).getOrElse("")
		val sub_group_name = (data \ "sub_group_name").asOpt[String].get
		
		val cg = from db() in "groups" where ("group_id" -> group_id) select (x => x)
		if (cg.count != 0) ErrorCode.errorToJson("group is not exist")
		else {
			val sb = cg.head.get("sub_groups").asInstanceOf[BasicDBList] 
			(sb.find(x => x.asInstanceOf[BasicDBObject].get("sub_group_id") == sub_group_id)).map { y =>
				ErrorCode.errorToJson("sub group is already exist")
			}.getOrElse {
			
				val date = new Date().getTime
				val tmp_builder = MongoDBObject.newBuilder
			  	tmp_builder += "sub_group_id" -> Sercurity.md5Hash(sub_group_name)
			  	tmp_builder += "sub_group_name" -> sub_group_name
			  	tmp_builder += "sub_group_found_time" -> date
			  	tmp_builder += "sub_group_update_time" -> date
			  	
			  	sb.add(tmp_builder.result)
			  	val g = cg.head
			  	g += "sub_groups" -> sb
			  	_data_connection.getCollection("groups").update(DBObject("group_id" -> group_id), g)
			
			  	queryGroups(data)
			}
		}
	}
	
	def canUserCreateChatGroup(user_id : String) : Boolean = true // TODO: add some rules 
  
	def queryGroups(data : JsValue) : JsValue = {
		def initGroupsForFirstTimeQuery = {
			("group_name_1" :: "group_name_2" :: "group_name_3" :: "group_name_4" :: "group_name_5" :: Nil) map { x => 
			  	val date = new Date().getTime
			  	val builder = MongoDBObject.newBuilder
			  	builder += "group_id" -> Sercurity.md5Hash(x)
			  	builder += "group_name" -> x
			  	builder += "group_found_time" -> date
			  	
			  	val list_builder = MongoDBList.newBuilder
			  	("sub_group1" :: Nil) map { y =>
			  		val tmp_builder = MongoDBObject.newBuilder
			  		tmp_builder += "sub_group_id" -> Sercurity.md5Hash(y)
			  		tmp_builder += "sub_group_name" -> y
			  		tmp_builder += "sub_group_found_time" -> date
			  		tmp_builder += "sub_group_update_time" -> date

			  		list_builder += tmp_builder.result
			  	}
			  	builder += "sub_groups" -> list_builder.result
			  	
				_data_connection.getCollection("groups") += builder.result
			}
		}

		if (!_data_connection.isExisted("groups")) initGroupsForFirstTimeQuery
	
		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
		
//		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
//		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
//		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)
	
		val result = from db() in "groups" select (x => x)
		var xls : List[JsValue] = Nil
		
		result.toList map { x => 
			var tmp : Map[String, JsValue] = Map.empty
			List("group_id", "group_name", "group_found_time", "sub_groups") map (iter => tmp += iter -> helpOptions.opt_2_js(Some(x.asDBObject.get(iter)), iter))
			xls = xls :+ toJson(tmp)
		}
		
		Json.toJson(Map("status" -> toJson("ok"),  "result" -> toJson(Map("groups" -> toJson(xls)))))
	}
}