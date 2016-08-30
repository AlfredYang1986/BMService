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

import module.kidnap.kidnapModule

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import akka.util.Timeout

import module.webpay.WechatPayModule

object orderStatus {
    case object reject extends orderStatusDefines(-9, "reject")
    case object unpaid extends orderStatusDefines(-1, "unpaid")
    case object ready extends orderStatusDefines(0, "ready")
    case object confirm extends orderStatusDefines(1, "confirm")
    case object paid extends orderStatusDefines(2, "paid")
    case object done extends orderStatusDefines(9, "done")
}

sealed abstract class orderStatusDefines(val t : Int, val des : String)

object orderModule {
 
    def JsValue2DB(data : JsValue, order_id : String) : MongoDBObject = {
        val builder = MongoDBObject.newBuilder
      
        val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
        val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
        
        builder += "user_id" -> user_id
        builder += "service_id" -> service_id

        val service = kidnapModule.queryKidnapServiceDetail(toJson(Map("service_id" -> toJson(service_id))))
        (service \ "status").asOpt[String].map { status => 
          if (status == "error") throw new Exception("service not valid") 
          else builder += "owner_id" -> (service \ "result" \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
        }
        
        builder += "date" -> new Date().getTime
        builder += "status" -> orderStatus.ready.t
        
        builder += "order_thumbs" -> (data \ "order_thumbs").asOpt[String].map (x => x).getOrElse(throw new Exception)
        builder += "order_title" -> (data \ "order_title").asOpt[String].map (x => x).getOrElse(throw new Exception)

        val order_date = MongoDBObject.newBuilder
        (data \ "order_date").asOpt[JsValue].map { x => 
            order_date += "start" -> (x \ "start").asOpt[Long].map (y => y).getOrElse(0.longValue)    
            order_date += "end" -> (x \ "end").asOpt[Long].map (y => y).getOrElse(0.longValue)    
        }.getOrElse(throw new Exception)
        builder += "order_date" -> order_date.result
        builder += "is_read" -> (data \ "is_read").asOpt[Int].map (x => x).getOrElse(0)
        builder += "order_id" -> order_id
        
        builder.result
    }
    
    def DB2JsValue(x : MongoDBObject) : JsValue = {
        val service = kidnapModule.queryKidnapServiceDetail(toJson(Map("service_id" -> toJson(x.getAs[String]("service_id").get))))
        (service \ "status").asOpt[String].map { status => 
          if (status == "error") throw new Exception("service not valid") 
          else {
            toJson(Map("user_id" -> toJson(x.getAs[String]("user_id").get),
                       "service_id" -> toJson(x.getAs[String]("service_id").get),
                       "owner_id" -> toJson(x.getAs[String]("owner_id").get),
                       "date" -> toJson(x.getAs[Long]("date").get),
                       "status" -> toJson(x.getAs[Int]("status").get),
                       "order_thumbs" -> toJson(x.getAs[String]("order_thumbs").get),
                       "order_date" -> toJson(Map("start" -> toJson(x.getAs[MongoDBObject]("order_date").get.getAs[Long]("start").get),
                                                  "end" -> toJson(x.getAs[MongoDBObject]("order_date").get.getAs[Long]("end").get))),
                       "is_read" -> toJson(x.getAs[Int]("is_read").get),
                       "order_id" -> toJson(x.getAs[String]("order_id").get),
                       "prepay_id" -> toJson(x.getAs[String]("prepay_id").map (x => x).getOrElse("")),
                       "service" -> (service \ "result")
                  ))
          }
        }.getOrElse(throw new Exception("wrong input"))
    }
  
    def pushOrder(data : JsValue) : JsValue = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
              
            val order_id = Sercurity.md5Hash(user_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
            val x = Future(JsValue2DB(data, order_id))
            val y = Future(WechatPayModule.prepayid(data))
            
            val obj = Await.result (x map (o => o), Timeout(1 second).duration).asInstanceOf[MongoDBObject]
            val js = Await.result (y map (v => v), Timeout(1 second).duration).asInstanceOf[JsValue]
           
            val prepay_id = (js \ "result" \ "prepay_id").asOpt[String].map (x => x).getOrElse("")
            
            obj += "prepay_id" -> prepay_id
            
            _data_connection.getCollection("orders") += obj
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(obj))))
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
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
       
    def queryOrder(data : JsValue) : JsValue = {
      
        def serviceIdCondition(v : String) = "service_id" $eq v
        def userIdCondition(u : String) = "user_id" $eq u
        def statusCondition(s : Int) = "status" $eq s
        def dateCondition(d : Long) = "date" $gte d
        def ownIdCondition(o : String) = "owner_id" $eq o
        def orderDateCondition(o : (Long, Long)) = $and("order_date.start" $gte o._1, "order_date.end" $lt o._2)
     
        def conditionsAcc(o : Option[DBObject], key : String, value : Any) : Option[DBObject] = {
            val n = key match {
              case "service_id" => serviceIdCondition(value.asInstanceOf[String])
              case "user_id" => userIdCondition(value.asInstanceOf[String])
              case "owner_id" => ownIdCondition(value.asInstanceOf[String])
              case "status" => statusCondition(value.asInstanceOf[Int])
              case "date" => dateCondition(value.asInstanceOf[Long])
              case "order_date" => orderDateCondition(value.asInstanceOf[(Long, Long)])
              case _ => ???
            }
            
            if (o.isEmpty) Option(n)
            else Option($and(o.get, n))
        }
        
        try { 
            var condition : Option[DBObject] = None
            (data \ "service_id").asOpt[String].map (x => condition = conditionsAcc(condition, "service_id", x)).getOrElse(Unit)
            (data \ "user_id").asOpt[String].map (x => condition = conditionsAcc(condition, "user_id", x)).getOrElse(Unit)
            (data \ "owner_id").asOpt[String].map (x => condition = conditionsAcc(condition, "owner_id", x)).getOrElse(Unit)
            (data \ "date").asOpt[Long].map (x => condition = conditionsAcc(condition, "date", x)).getOrElse(Unit)
            (data \ "status").asOpt[Int].map (x => condition = conditionsAcc(condition, "status", x)).getOrElse(Unit)
            (data \ "order_date").asOpt[JsValue].map (x => condition = conditionsAcc(condition, "order_date", ((x \ "start").asOpt[Long].get, (x \ "end").asOpt[Long].get))).getOrElse(Unit)
          
            if (condition.isEmpty) throw new Exception("wrong input")
            else toJson(Map("status" -> toJson("ok"), "result" -> toJson((from db() in "orders" where condition.get select (DB2JsValue(_))).toList)))
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def queryApplyOrder(data : JsValue) : JsValue = queryOrder(data)
    def queryOwnOrder(data : JsValue) : JsValue = queryOrder(data)
}