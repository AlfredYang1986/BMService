package module.order

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

import play.api.Play.current
import play.api.libs.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.util.Random

import module.sercurity.Sercurity
import java.util.Date

object orderStatus {
    case object ready extends orderStatusDefines(0, "ready")
    case object confirm extends orderStatusDefines(1, "confirm")
    case object done extends orderStatusDefines(2, "done")
}

sealed abstract class orderStatusDefines(val t : Int, val des : String)

object orderModule {
 
    def JsValue2DB(data : JsValue) : Option[MongoDBObject] = 
        try {
            val builder = MongoDBObject.newBuilder
          
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
            
            builder += "user_id" -> user_id
            builder += "service_id" -> service_id
           
            builder += "date" -> new Date().getTime
            builder += "status" -> orderStatus.ready.t
            
            builder += "order_thumbs" -> (data \ "order_thumbs").asOpt[String].map (x => x).getOrElse(throw new Exception)
            builder += "order_title" -> (data \ "order_title").asOpt[String].map (x => x).getOrElse(throw new Exception)

            builder += "order_date" -> (data \ "order_date").asOpt[Long].map (x => x).getOrElse(throw new Exception)
            builder += "is_read" -> (data \ "is_read").asOpt[Int].map (x => x).getOrElse(0)
           
            builder += "order_id" -> Sercurity.md5Hash(user_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
            
            Some(builder.result)
          
        } catch {
          case ex : Exception => None
        }
    
    def DB2JsValue(x : MongoDBObject) : JsValue = 
        toJson(Map("user_id" -> toJson(x.getAs[String]("user_id").get),
                   "service_id" -> toJson(x.getAs[String]("service_id").get),
                   "date" -> toJson(x.getAs[Long]("date").get),
                   "status" -> toJson(x.getAs[Int]("status").get),
                   "order_thumbs" -> toJson(x.getAs[String]("order_thumbs").get),
                   "order_date" -> toJson(x.getAs[Long]("order_date").get),
                   "is_read" -> toJson(x.getAs[Int]("is_read").get),
                   "order_id" -> toJson(x.getAs[String]("order_id").get)
              ))
  
    def pushOrder(data : JsValue) : JsValue = 
        JsValue2DB(data) match {
          case Some(x) => {
              _data_connection.getCollection("orders") += x
              toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(x))))
          }
          case None => ErrorCode.errorToJson("wrong input")
        }
    
    def popOrder(data : JsValue) : JsValue = 
        try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "orders" where ("order_id" -> order_id) select (x => x)).toList match {
              case head :: Nil => {
                  _data_connection.getCollection("orders") -= head
                  toJson(Map("status" -> toJson("ok"), "resutl" -> toJson(Map("order_id" -> order_id))))
              }
              case _ => ErrorCode.errorToJson("wrong input")
            }
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def updateOrder(data : JsValue) : JsValue = 
        try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "orders" where ("order_id" -> order_id) select (x => x)).toList match {
              case head :: Nil => {
                  (data \ "status").asOpt[Int].map (x => head += "status" -> x.intValue.asInstanceOf[Number])
                  (data \ "order_thumbs").asOpt[String].map (x => head += "order_thumbs" -> x)
                  (data \ "order_date").asOpt[Long].map (x => head += "order_date" -> x.longValue.asInstanceOf[Number])
                  (data \ "is_read").asOpt[Int].map (x => head += "is_read" -> x.intValue.asInstanceOf[Number])
                  
                  _data_connection.getCollection("orders").update(DBObject("order_id" -> order_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(head))))
              }
              case _ => ErrorCode.errorToJson("wrong input")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
}