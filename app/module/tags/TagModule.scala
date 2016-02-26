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

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import akka.util.Timeout
import scala.concurrent.duration._

sealed abstract class tag_type(val index : Int, val dis : String)
case object tag_type_location extends tag_type(0, "locationn tag")
case object tag_type_time extends tag_type(1, "time tag")
case object tag_type_tag extends tag_type(2, "tag tag")
case object tag_type_brand extends tag_type(3, "brand tag")

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

	def queryFoundSearchTagData(data : JsValue) : JsValue = {
			
		val f0 = Future(this.queryRecommandTags(data))
		val f1 = Future(this.queryTagPreViewWithTagName(data))
//		val f1 = Future(this.queryTagSearchWithInput(data))
		
		Await.result((f0 zip f1) map { x => 
			val result0 = if ((x._1 \ "status").asOpt[String].get.equals("ok")) (true, (x._1 \ "recommands"))
						  else (false, null)
			val result1 = if ((x._2 \ "status").asOpt[String].get.equals("ok")) (true, (x._2 \ "preview"))
						  else (false, null)
		  	if (result0._1 && result1._1) toJson(Map("status" -> toJson("ok"), "recommands" -> result0._2, "preview" -> result1._2))
		  	else ErrorCode.errorToJson("unknown error")
		}, Timeout(2 second).duration).asInstanceOf[JsValue]
	}
	
	def queryRecommandTags(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
	
		(data \ "tag_type").asOpt[Int].map { tp => 
		  Json.toJson(Map("status" -> toJson("ok"), "recommands" -> toJson((from db() in "tags" where ("type" -> tp)).selectTop(10)("date"){ x => 
			  toJson(Map("tag_name" -> toJson(x.getAs[String]("content").get), "tag_type" -> toJson(x.getAs[Number]("type").get.intValue)))
  		}.toList))).asInstanceOf[JsValue]
		
		}.getOrElse{
			Json.toJson(Map("status" -> toJson("ok"), "recommands" -> toJson((from db() in "tags").selectTop(10)("date"){ x => 
			  toJson(Map("tag_name" -> toJson(x.getAs[String]("content").get), "tag_type" -> toJson(x.getAs[Number]("type").get.intValue)))
  		}.toList))).asInstanceOf[JsValue]
		}
	}

	def queryTagSearchWithInput(data : JsValue) : JsValue = {
		
	  val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val tag_name = (data \ "tag_name").asOpt[String].get
		
		(data \ "tag_type").asOpt[Int].map { tag_type => 
			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(
  		  (from db() in "tags" where ("content" $regex ("(?i)" + tag_name), "type" -> tag_type)).select { x => 
  		    toJson(Map("tag_name" -> toJson(x.getAs[String]("content").get), "tag_type" -> toJson(x.getAs[Number]("type").get.intValue)))
  		  }.toList)))
		}.getOrElse {
			Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(
    		(from db() in "tags" where ("content" $regex ("(?i)" + tag_name))).select { x => 
  		    toJson(Map("tag_name" -> toJson(x.getAs[String]("content").get), "tag_type" -> toJson(x.getAs[Number]("type").get.intValue)))
  		  }.toList)))
		}
	}
	
	def queryTagPreViewWithTagName(data : JsValue) : JsValue = {
	  
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val tag_name = (data \ "tag_name").asOpt[String].get
		val date = (data \ "date").asOpt[Long].map(x => x).getOrElse(new Date().getTime)

    def getPreViewTagName(o : MongoDBObject) : String =
		    o.getAs[MongoDBList]("tags").map { x => x.map { x =>
		        x.asInstanceOf[BasicDBObject].get("content").asInstanceOf[String]}.filter (str => str.startsWith(tag_name)).head
		    }.getOrElse("")
		
		def queryPreViewWithTagType(tag : String, t : Integer) : JsValue = {
  			((from db() in "posts" where ("tags" $elemMatch($and("content" $eq tag, "type" $eq t.intValue)))).selectTop(3)("date") { x =>
  				  toJson(Map(
				        "post_id" -> toJson(x.getAs[String]("post_id").get), 
    				    "items" -> toJson(
		  		        ((x.getAs[MongoDBList]("items").get.toSeq) map { y => y match {
				            case item : BasicDBObject=>
				            	toJson(Map("name" -> toJson(item.get("name").asInstanceOf[String]), "type" -> toJson(item.get("type").asInstanceOf[Number].intValue)))
				            case _ => ???
				          }}).toList)))
			  }).toList match {
			    case Nil => null
			    case x : List[JsValue] => println(x); toJson(Map("tag_name" -> toJson(tag), "type" -> toJson(t.intValue), "content" -> toJson(x)))
			}
		}
		
		val user_check = from db() in "users" where ("user_id" -> user_id) select (x => x)
		if (user_check.count == 0) ErrorCode.errorToJson("user not existing")
		else {
		  var result : List[JsValue] = Nil
      val tag = ((from db() in "tags" where ("content" $regex ("(?i)" + tag_name))).select(x => x.getAs[String]("content").get)).toList
      println(tag)
      tag.map { x => result = (queryPreViewWithTagType(x, tag_type_location.index) :: 
				    	queryPreViewWithTagType(x, tag_type_time.index) ::
				    	queryPreViewWithTagType(x, tag_type_tag.index) ::
				    	queryPreViewWithTagType(x, tag_type_brand.index) :: Nil)
				    	.filterNot(_ == null) ::: result}
		    
			toJson(Map("status" -> toJson("ok"), "preview" -> 
				toJson(result)))
		}
	}
}