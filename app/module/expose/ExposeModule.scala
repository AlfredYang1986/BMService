package module.expose

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity

object exposeTypes {
    case object exposeUser extends exposeTypeDefines(0, "user")
    case object exposeContent extends exposeTypeDefines(1, "content")
}

sealed abstract class exposeTypeDefines(val t : Int, val des : String) 

object ExposeModule {
    
    def createExposeDBObject(data : JsValue) : Option[MongoDBObject] =
        try {
            val builder = MongoDBObject.newBuilder
            
            (data \ "expose_type").asOpt[Int].map (tmp => builder += "expose_type" -> tmp).getOrElse(throw new Exception("wrong input"))
            (data \ "post_id").asOpt[String].map (tmp => builder += "post_id" -> tmp).getOrElse(builder += "post_id" -> "")
            (data \ "user_id").asOpt[String].map (tmp => builder += "user_id" -> tmp).getOrElse(builder += "user_id" -> "")
            builder += "expose_id" -> Sercurity.md5Hash("expose" + Sercurity.getTimeSpanWithMillSeconds)
            
            Some(builder.result)
        } catch {
          case ex : Exception => None
        }
    
    def DB2JsValue(obj : MongoDBObject) : JsValue = 
        toJson(Map("expose_type" -> toJson(obj.getAs[Number]("expose_type").get.intValue),
                   "post_id" -> toJson(obj.getAs[String]("post_id").get),
                   "user_id" -> toJson(obj.getAs[String]("user_id").get),
                   "expose_id" -> toJson(obj.getAs[String]("expose_id").get)))
  
    def expose(data : JsValue) : JsValue =
        createExposeDBObject(data) match {
          case None => ErrorCode.errorToJson("wrong input")
          case Some(x) => {
              _data_connection.getCollection("expose") += x 
              toJson(Map("status" -> toJson("ok"), "result"-> toJson(DB2JsValue(x))))
          }
        }
}