package module.kidnap

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

object kidnapModule {
  
    def queryServiceStatus(service_id : Option[String]) : (Int, MongoDBObject) = {
        service_id match {
          case Some(x) => (from db() in "kidnap" where ("service_id" -> x) select (tmp => tmp)).toList match {
                            case Nil => (kidnapServiceStatus.none.t, null)
                            case head :: Nil => (head.getAs[Number]("status").get.intValue, head)
                            case _ => null
                          }
          case None => (kidnapServiceStatus.none.t, null)
        }
    }
  
  	def pushKidnapService(data : JsValue) : JsValue = {
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).push(data, origin)
  	}
  	
  	def pushKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : JsValue = {
  	    try {
      	    val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("user not existing"))
  	        val service_id = Sercurity.md5Hash(owner_id + Sercurity.getTimeSpanWithMillSeconds)
  	        
  	        val service_builder = MongoDBObject.newBuilder
  	        service_builder += "service_id" -> service_id
  	        service_builder += "owner_id" -> owner_id
  	        
  	        val offer_date = MongoDBObject.newBuilder
  	        (data \ "offer_date").asOpt[JsValue].map { date => 
  	            offer_date += "start" -> (date \ "start").asOpt[Long].map (tmp => tmp).getOrElse(0.longValue)   
  	            offer_date += "end" -> (date \ "end").asOpt[Long].map (tmp => tmp).getOrElse(0.longValue)   
  	        }.getOrElse(throw new Exception("wrong input"))
  	        service_builder += "offer_date" -> offer_date.result
  	       
  	        val location = MongoDBObject.newBuilder
  	        (data \ "location").asOpt[JsValue].map { loc => 
  	            location += "latitude" -> (loc \ "latitude").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	            location += "longtitude" -> (loc \ "longtitude").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	        }.getOrElse(throw new Exception("wrong input"))
  	        service_builder += "location" -> location.result
  	        
  	        service_builder += "comments" -> MongoDBList.newBuilder.result
  	        (data \ "title").asOpt[String].map (tmp => service_builder += "title" -> tmp).getOrElse(throw new Exception("wrong input"))
  	        (data \ "description").asOpt[String].map (tmp => service_builder += "description" -> tmp).getOrElse(service_builder += "description" -> "")
  	        (data \ "capacity").asOpt[Int].map (tmp => service_builder += "capacity" -> tmp).getOrElse(service_builder += "capacity" -> 0.intValue)
  	        (data \ "price").asOpt[Float].map (tmp => service_builder += "price" -> tmp).getOrElse(service_builder += "price" -> 0.floatValue)

  	        service_builder += "status" -> kidnapServiceStatus.offine.t
  	        service_builder += "rate" -> 0.floatValue
  	      
  	        (data \ "cans").asOpt[Long].map (cans => service_builder += "cans" -> cans.asInstanceOf[Number]).getOrElse(service_builder += "cans" -> 0.intValue)
            (data \ "facility").asOpt[Long].map (cans => service_builder += "facility" -> cans.asInstanceOf[Number]).getOrElse(service_builder += "facility" -> 0.intValue)
  	       
            (data \ "images").asOpt[List[String]].map { lst => 
                service_builder += "images" -> lst
            }.getOrElse(service_builder += "images" -> MongoDBList.newBuilder.result) 
  	        
  	        service_builder += "reserve1" -> ""
  	        
  	        _data_connection.getCollection("kidnap") += service_builder.result
  	        
  	        toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	    
  	}
  	
  	def popKidnapService(data : JsValue) : JsValue = {
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).pop(data, origin)
  	}
  	
  	def popKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : JsValue = {
  	    try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
            origin += "status" -> kidnapServiceStatus.removed.t.asInstanceOf[Number]
            _data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), origin)

            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	}
  	
  	def updateKidnapService(data : JsValue) : JsValue = { 
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).update(data, origin)
  	}
  	
  	def updateKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : JsValue = {
       
  	    try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
           
            (data \ "title").asOpt[String].map (x => origin += "title" -> x).getOrElse(Unit)
            (data \ "description").asOpt[String].map (x => origin += "description" -> x).getOrElse(Unit)
            (data \ "capacity").asOpt[Int].map (x => origin += "capacity" -> x.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "price").asOpt[Float].map (x => origin += "price" -> x.asInstanceOf[Number]).getOrElse(Unit)

            (data \ "location").asOpt[JsValue].map { loc =>
                val location = MongoDBObject.newBuilder
  	            location += "latitude" -> (loc \ "latitude").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	            location += "longtitude" -> (loc \ "longtitude").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	            origin += "location" -> location.result            
            }.getOrElse(Unit)
            
            (data \ "offer_date").asOpt[JsValue].map { date =>
                val offer_date = MongoDBObject.newBuilder
  	            offer_date += "start" -> (date \ "start").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	            offer_date += "end" -> (date \ "end").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	            origin += "offer_date" -> offer_date.result            
            }.getOrElse(Unit)
           
            (data \ "cans").asOpt[Long].map (cans => origin += "cans" -> cans.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "facility").asOpt[Long].map (cans => origin += "facility" -> cans.asInstanceOf[Number]).getOrElse(Unit)
  	       
            (data \ "images").asOpt[List[String]].map { lst => 
                origin += "images" -> lst
            }.getOrElse(Unit) 
            
            _data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), origin)

            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	}
  	
  	def publishKidnapService(data : JsValue) : JsValue = {
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).publish(data, origin)
  	}
  	
  	def publishKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : JsValue = {
  	    try {
  	        val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
  	        origin += "status" -> kidnapServiceStatus.online.t.asInstanceOf[Number]
            _data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), origin)

            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	}
  	
  	def revertKidnapService(data : JsValue) : JsValue = { 
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).revert(data, origin)
  	}
  	
  	def revertKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : JsValue = {
  	    try {
  	        val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
  	        origin += "status" -> kidnapServiceStatus.offine.t.asInstanceOf[Number]
            _data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), origin)

            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	}
  	
  	def DB2JsValue(x : MongoDBObject) : JsValue = 
  	    if (x == null) ErrorCode.errorToJson("service not existing")
  	    else toJson(Map("service_id" -> toJson(x.getAs[String]("service_id").get),
  	               "title" -> toJson(x.getAs[String]("title").get),
  	               "description" -> toJson(x.getAs[String]("description").get),
  	               "capacity" -> toJson(x.getAs[Number]("capacity").get.intValue),
  	               "price" -> toJson(x.getAs[Number]("price").get.floatValue),
  	               "owner_id" -> toJson(x.getAs[String]("owner_id").get),
  	               "offer_date" -> toJson(Map("start" -> toJson(x.getAs[MongoDBObject]("offer_date").get.getAs[Number]("start").get.longValue),
  	                                          "end" -> toJson(x.getAs[MongoDBObject]("offer_date").get.getAs[Number]("end").get.longValue))),
  	               "location" -> toJson(Map("latitude" -> toJson(x.getAs[MongoDBObject]("location").get.getAs[Number]("latitude").get.floatValue),
  	                                        "longtitude" -> toJson(x.getAs[MongoDBObject]("location").get.getAs[Number]("longtitude").get.floatValue))),
  	               "cans" -> toJson(x.getAs[Number]("cans").get.longValue),
  	               "facility" -> toJson(x.getAs[Number]("facility").get.longValue),
  	               "images" -> toJson(x.getAs[MongoDBList]("images").get.toList.asInstanceOf[List[String]])
  	               ))
  
  	def queryKidnapServiceDetail(data : JsValue) : JsValue = {
  	    try {
  	        val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
  	     
      	    toJson(Map("status" -> toJson("ok"), "result" -> toJson(
      	        (from db() in "kidnap" where ("service_id" -> service_id) select (DB2JsValue(_))).head)))
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	}
  	               
  	def searchKidnapService(data : JsValue) : JsValue = {
  	    try {
  	        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(0.intValue)
  	        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0.intValue)
  	      
  	        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
      	        (from db() in "kidnap" where ("status" -> kidnapServiceStatus.online.t)).
      	            selectSkipTop(skip)(take)("offer_date.start")(DB2JsValue(_)).toList)))
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	}
  	
  	def mineKidnapService(data : JsValue) : JsValue = {
  	    try {
  	        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

  	        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(0.intValue)
  	        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0.intValue)
  	       
  	        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
      	        (from db() in "kidnap" where ("owner_id" -> owner_id)).
      	            selectSkipTop(skip)(take)("offer_date.start")(DB2JsValue(_)).toList)))
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	}
}