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

object QueryModule {
  
	/**
	 * for the initial stage, only can query yours data
	 */
	def queryHomeContent(data : JsValue) : JsValue = {

		def opt_2_js(value : Option[AnyRef]) : JsValue = { 
			def opt_str_2_js(value : String) : JsValue = toJson(value)
			def opt_val_2_js(value : Number) : JsValue = toJson(value.longValue())
			def opt_map_2_js(value : BasicDBList) : JsValue = {
				var xls : List[JsValue] = Nil
				value.map { x =>
					var tmp : Map[String, JsValue] = Map.empty
					List("type", "name") map { iter =>
				  		x.asInstanceOf[BasicDBObject].get(iter) match {
				  			case str : String => println(str); tmp += iter -> opt_str_2_js(str)
				  			case list : BasicDBList => println(list); tmp += iter -> opt_map_2_js(list)
				  			case n : Number => println(n); tmp += iter -> opt_val_2_js(n)
				  			case _ => Unit
				  		}
					}			  
				  	xls = xls :+ toJson(tmp)
				}
				Json.toJson(xls)
			}
	  
		  	value.map ( x => x match {
		  	  	case str : String => opt_str_2_js(str)
		  	  	case list : BasicDBList => opt_map_2_js(list)
		  	  	case n : Number => opt_val_2_js(n)
		  	  	case _ => ??? 
		  	  }
		  	).getOrElse(toJson(""))
		}

		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id = (data \ "user_id").asOpt[String].get
		
		val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
		val skip = (data \ "skip").asOpt[Int].map(x => x).getOrElse(0)
//		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(50)
		val take = (data \ "take").asOpt[Int].map(x => x).getOrElse(2)

		var xls : List[JsValue] = Nil
		(from db() in "posts" where ("date" $lte date)).selectTop(take)("date") { x => 
		  	var tmp : Map[String, JsValue] = Map.empty
		  	List("date", "owner_id", "owner_name", "title", "description", "likes", "items") map (iter => tmp += iter -> opt_2_js(x.get(iter)))
		  	xls = xls :+ toJson(tmp)
		}

		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(xls)))
	}
	
	def downloadFile(name : String) : Array[Byte] = fop.downloadFile(name)
}