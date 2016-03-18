package module.common

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue
import com.mongodb.casbah.Imports._
  
object helpOptions {
	def opt_2_js(value : Option[AnyRef], key : String) : JsValue = { 
		def opt_str_2_js(value : String) : JsValue = toJson(value)
		def opt_val_2_js(value : Number) : JsValue = if (value.isInstanceOf[java.lang.Double]) toJson(value.doubleValue)
		                                             else toJson(value.longValue)
		def opt_map_2_js(value : BasicDBList, key : String) : JsValue = {
			def key2List : List[String] = key match {
				case "items" => List("type", "name")
				case "tags" => List("type", "content", "offsetX", "offsetY")
				case "comments" => List("comment_owner_id", "comment_owner_name", "comment_date", "comment_content", "comment_owner_photo")
				case "likes" => List("like_owner_id", "like_owner_name", "like_owner_photo", "like_date")
				case "push" => List("push_owner_id", "push_owner_name", "push_owner_photo", "push_date")
				case "gourps" => List("group_id", "group_name", "group_found_time")
				case "sub_groups" => List("sub_group_id", "sub_group_name", "sub_group_found_time", "sub_group_update_time")
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
	
	def opt_2_js_2(value : Option[AnyRef], key : String)(func : String => JsValue) : JsValue = { 
		def opt_str_2_js(value : String) : JsValue = toJson(value)
		def opt_val_2_js(value : Number) : JsValue = if (value.isInstanceOf[java.lang.Double]) toJson(value.doubleValue)
		                                             else toJson(value.longValue)
		def opt_map_2_js(value : BasicDBList, key : String) : JsValue = {
			def key2List : List[String] = key match {
				case "items" => List("type", "name")
				case "tags" => List("type", "content", "offsetX", "offsetY")
				case "comments" => List("comment_owner_id", "comment_owner_name", "comment_date", "comment_content", "comment_owner_photo")
				case "likes" => List("like_owner_id", "like_owner_name", "like_owner_photo", "like_date")
				case "gourps" => List("group_id", "group_name", "group_found_time")
				case "sub_groups" => List("sub_group_id", "sub_group_name", "sub_group_found_time", "sub_group_update_time")
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
		}).getOrElse(func(key))
	}
}