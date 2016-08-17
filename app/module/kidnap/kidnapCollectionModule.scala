package module.kidnap

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object kidnapCollectionModule {
    def collectKidnapService(data : JsValue) : JsValue = 
        try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            def pushUserService : Boolean = 
                (from db() in "user_service" where ("user_id" -> user_id) select (x => x)).toList match {  
                  case Nil => {
                      val builder = MongoDBObject.newBuilder
                      builder += "user_id" -> user_id
                      builder += "services" -> MongoDBList(service_id :: Nil)
                      
                      _data_connection.getCollection("user_service") += builder.result
                      true
                  }
                  case head :: Nil => {
                      val service_lst = head.getAs[MongoDBList]("services").get.toList.asInstanceOf[List[String]]
                      head += "services" -> MongoDBList((service_lst +: service_id).distinct)
                      _data_connection.getCollection("user_service").update(DBObject("user_id" -> user_id), head)
                      true
                  }
                  case _ => ???
                }
            
            def pushServiceUser : Boolean = 
                (from db() in "service_user" where ("service_id" -> service_id) select (x => x)).toList match {  
                  case Nil => {
                      val builder = MongoDBObject.newBuilder
                      builder += "service_id" -> service_id
                      builder += "users" -> MongoDBList(user_id :: Nil)
                      
                      _data_connection.getCollection("service_user") += builder.result
                      true
                  }
                  case head :: Nil => {
                      val user_lst = head.getAs[MongoDBList]("users").get.toList.asInstanceOf[List[String]]
                      head += "users" -> MongoDBList((user_lst +: user_id).distinct)
                      _data_connection.getCollection("service_user").update(DBObject("service_id" -> service_id), head)
                      true
                  }
                  case _ => ???
                }
         
            if (pushUserService && pushServiceUser) toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
            else throw new Exception("something wrong")    
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def unCollectKidnapService(data : JsValue) : JsValue = 
        try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
           
            def popUserService : Boolean = 
                (from db() in "user_service" where ("user_id" -> user_id) select (x => x)).toList match {
                  case Nil => throw new Exception("wrong input")
                  case head :: Nil => {
                      head += "services" -> head.getAs[MongoDBList]("services").get.toList.asInstanceOf[List[String]].filterNot (service_id.equals(_))
                      _data_connection.getCollection("user_service").update(DBObject("user_id" -> user_id), head)
                      true
                  }
                  case _ => ???
                }
            
            def popServiceUser : Boolean = {
                (from db() in "service_user" where ("service_id" -> service_id) select (x => x)).toList match {
                  case Nil => throw new Exception("wrong input")
                  case head :: Nil => {
                      head += "users" -> head.getAs[MongoDBList]("users").get.toList.asInstanceOf[List[String]].filterNot (user_id.equals(_))
                      _data_connection.getCollection("service_user").update(DBObject("service_id" -> service_id), head)
                      true                   
                  }
                  case _ => ???
                }
            }
            
            if (popUserService && popServiceUser) toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
            else throw new Exception("something wrong")
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def userCollectionsLst(data : JsValue) : JsValue = 
        try {
            println(data)
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            println(user_id)
            val lst = (from db() in "user_service" where ("user_id" -> 
                        (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))) select (x => 
                          x.getAs[MongoDBList]("services").get.toList.asInstanceOf[List[String]])).toList.head.distinct
            println(lst)
                          
            kidnapModule.queryMultipleService(toJson(Map("lst" -> lst)))
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
}