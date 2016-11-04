package module.order

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

import java.util.Date
import module.sercurity.Sercurity

object orderCommentsModule {
    def pushComments(data : JsValue) : JsValue = {
        try {
            val builder = MongoDBObject.newBuilder
            
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            builder += "order_id" -> order_id
            builder += "service_id" -> service_id
            builder += "owner_id" -> (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "user_id" -> (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "content" -> (data \ "content").asOpt[String].map (x => x).getOrElse("")
            builder += "points" -> ((data \ "points").asOpt[List[Int]].map (x => x).getOrElse(throw new Exception("wrong input"))).map (_.toFloat)
            
            builder += "comment_id" -> Sercurity.md5Hash(order_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
            builder += "date" -> new Date().getTime
            
            _data_connection.getCollection("service_comments") += builder.result
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2Object(builder.result))))
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def updateComments(data : JsValue) : JsValue = {
        try {
            val comment_id = (data \ "comment_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            (from db() in "service_comments" where ("comment_id" -> comment_id) select (x => x)).toList match {
              case head :: Nil => {
//                  (data \ "accuracy").asOpt[Float].map (x => head += "accuracy" -> x.asInstanceOf[Number]).getOrElse(Unit)
//                  (data \ "communication").asOpt[Float].map (x => head += "communication" -> x.asInstanceOf[Number]).getOrElse(Unit)
//                  (data \ "professional").asOpt[Float].map (x => head += "professional" -> x.asInstanceOf[Number]).getOrElse(Unit)
//                  (data \ "hygiene").asOpt[Float].map (x => head += "hygiene" -> x.asInstanceOf[Number]).getOrElse(Unit)

                  (data \ "points").asOpt[List[Float]].map (x => head += "points" -> x).getOrElse(Unit)
                  (data \ "content").asOpt[String].map (x => head += "content" -> x).getOrElse(Unit)
                 
                  _data_connection.getCollection("service_comments").update(DBObject("comment_id" -> comment_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2Object(head))))
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def popComments(data : JsValue) : JsValue = {
        try {
            val comment_id = (data \ "comment_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "service_comments" where("comment_id" -> comment_id) select (x => x)).toList match {
              case head :: Nil => {
                  _data_connection.getCollection("service_comments") -= head
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2Object(head))))
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
   
    def queryOverallComments(data : JsValue) : JsValue = {
     
    	def average(lst : List[Double], count : Int) : List[Double] = lst map (x => x / count)
    	
        def overallAcc(r : List[Double], lst : List[List[Double]]) : List[Double] = lst match { 
          case Nil => r
          case head :: Nil => r match {
            case Nil => head
            case _ => (r zip head).map (x => x._1 + x._2)
          }
        }
      
        try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
   
            val result = (from db() in "service_comments" where ("service_id" -> service_id) select (x => x.getAs[MongoDBList]("points").get.toList.asInstanceOf[List[Double]])).toList
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id), "points" -> toJson(
                        average(overallAcc(Nil, result), result.length)
                        )))))
        } catch {
          case ex : Exception => ex.printStackTrace(); ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def queryComments(data : JsValue) : JsValue = {
        try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
       
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                      (from db() in "service_comments" where ("service_id" -> service_id, "order_id" -> order_id) select (DB2Object(_))).toList)))
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def DB2Object(x : MongoDBObject) : JsValue = 
        toJson(Map("comment_id" -> toJson(x.getAs[String]("comment_id").get),
                   "order_id" -> toJson(x.getAs[String]("order_id").get),
                   "owner_id" -> toJson(x.getAs[String]("owner_id").get),
                   "user_id" -> toJson(x.getAs[String]("user_id").get),
                   "service_id" -> toJson(x.getAs[String]("service_id").get),
                   "content" -> toJson(x.getAs[String]("content").get),
                   "points" -> toJson(x.getAs[List[Float]]("points").get.toList),
                   "date" -> toJson(x.getAs[Number]("date").get.longValue)))
}