package module.profile

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.common.helpOptions
import module.relationship._

import java.util.Date

//import akka.actor.{Actor, Props}
//import play.api.libs.concurrent.Akka
//import play.api.GlobalSettings
//import play.api.templates.Html
//import play.api.libs.concurrent.Execution.Implicits.defaultContext

sealed abstract class horoscope(val index : Int)
case object Aries extends horoscope(0) 
case object Taurus extends horoscope(1) 
case object Gemini extends horoscope(2) 
case object Cancer extends horoscope(3) 
case object Leo extends horoscope(4) 
case object Virgo extends horoscope(5) 
case object Libra extends horoscope(6) 
case object Scorpio extends horoscope(7) 
case object Sagittarius extends horoscope(8) 
case object Capricorn extends horoscope(9) 
case object Aquarius extends horoscope(10) 
case object Pisces extends horoscope(11) 

sealed abstract class gender(val index : Int)
case object Male extends gender(0)
case object Female extends gender(1)

object DetailProfileModule {
  
	def createAndUpdateDetailDescription(data : JsValue) : JsValue = {
	 
	  	val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		val rel = from db() in "user_profile" where ("user_id" -> user_id) select (x => x)
		if (rel.count != 1) ErrorCode.errorToJson("unknown user")
		else {
			val user = rel.head

			(data \ "role_tag").asOpt[String].map (x => user += "role_tag" -> x).getOrElse(Unit)
			(data \ "hometown").asOpt[String].map (x => user += "hometown" -> x).getOrElse(Unit)
			(data \ "dob").asOpt[Long].map (x => user += "dob" -> x.longValue.asInstanceOf[java.lang.Long]).getOrElse(Unit)
			(data \ "school").asOpt[String].map (x => user += "school" -> x).getOrElse(Unit)
			(data \ "gender").asOpt[Int].map (x => user += "gender" -> x.intValue.asInstanceOf[java.lang.Integer]).getOrElse(Unit)
			
			(data \ "kids").asOpt[List[JsValue]].map { lst => 
			  	val kids_builder = MongoDBList.newBuilder
				lst.foreach { iter => 
				  	val builder = MongoDBObject.newBuilder
				  	(iter \ "dob").asOpt[Long].map (x => builder += "dob" -> x.longValue.asInstanceOf[java.lang.Long]).getOrElse(builder += "dob" -> -1.asInstanceOf[java.lang.Long])
				  	(iter \ "horoscope").asOpt[Int].map (x => user += "horoscope" -> x.intValue.asInstanceOf[java.lang.Integer]).getOrElse(user += "horoscope" -> -1.asInstanceOf[java.lang.Integer])
				  	(iter \ "school").asOpt[String].map (x => user += "school" -> x).getOrElse(user += "horoscope" -> "")
				  	(iter \ "gender").asOpt[Int].map (x => user += "gender" -> x.intValue.asInstanceOf[java.lang.Integer]).getOrElse(user += "gender" -> -1.asInstanceOf[java.lang.Integer])
				  	kids_builder += builder.result
				  	}
			  	user += "kids" -> kids_builder.result
			}.getOrElse(Unit)

			_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
		}
	  
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("create and update success")))
	}
  
	def queryDetailDescription(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
	
		val rel = from db() in "user_profile" where ("user_id" -> user_id) select { x => 
			var tmp : Map[String, JsValue] = Map.empty
			x.getAs[String]("role_tag").map (y => tmp += "role_tag" -> toJson(y)).getOrElse(Unit)
			x.getAs[Long]("dob").map (y => tmp += "dob" -> toJson(y)).getOrElse(Unit)
			x.getAs[String]("hometown").map (y => tmp += "hometown" -> toJson(y)).getOrElse(Unit)
			x.getAs[Int]("horoscope").map (y => tmp += "horoscope" -> toJson(y)).getOrElse(Unit)
			x.getAs[String]("school").map (y => tmp += "school" -> toJson(y)).getOrElse(Unit)
			x.getAs[Int]("gender").map (y => tmp += "gender" -> toJson(y)).getOrElse(Unit)

			x.getAs[MongoDBList]("kids").map { kids => 
				var kids_lst : List[JsValue] = Nil
				kids.foreach { kid => 
					var kid_tmp : Map[String, JsValue] = Map.empty
					kid.asInstanceOf[BasicDBObject].getAs[Long]("dob").map (y => kid_tmp += "dob" -> toJson(y)).getOrElse(Unit)
					println(kid_tmp)
					kid.asInstanceOf[BasicDBObject].getAs[String]("school").map (y => kid_tmp += "school" -> toJson(y)).getOrElse(Unit)
					println(kid_tmp)
					kid.asInstanceOf[BasicDBObject].getAs[Int]("gender").map (y => kid_tmp += "gender" -> toJson(y)).getOrElse(Unit)
					println(kid_tmp)
					kid.asInstanceOf[BasicDBObject].getAs[Int]("horoscope").map (y => kid_tmp += "horoscope" -> toJson(y)).getOrElse(Unit)
					println(kid_tmp)
					kids_lst = toJson(kid_tmp):: kids_lst
				
				}
				tmp += "kids" -> toJson(kids_lst)
			}.getOrElse(Unit)
			toJson(tmp)
		}
	
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(rel.head.asInstanceOf[JsValue])))
	}
}