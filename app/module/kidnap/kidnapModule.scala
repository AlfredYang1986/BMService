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
import module.profile.ProfileModule

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
  	        }.getOrElse {
  	            offer_date += "start" -> 0.longValue
  	            offer_date += "end" -> 0.longValue
  	        }
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
  	       
            service_builder += "distinct" -> (data \ "distinct").asOpt[String].map (x => x).getOrElse("")
            service_builder += "address" -> (data \ "address").asOpt[String].map (x => x).getOrElse("")
            service_builder += "adjust_address" -> (data \ "adjust_address").asOpt[String].map (x => x).getOrElse("")
            
            val age_boundary = MongoDBObject.newBuilder
            (data \ "age_boundary").asOpt[JsValue].map { boundary => 
                age_boundary += "lsl" -> (boundary \ "lsl").asOpt[Int].map (x => x).getOrElse(3)
                age_boundary += "usl" -> (boundary \ "usl").asOpt[Int].map (x => x).getOrElse(11)
            }.getOrElse {
  	            age_boundary += "lsl" -> 3.longValue
  	            age_boundary += "usl" -> 11.longValue
            }
  	        service_builder += "age_boundary" -> age_boundary.result
           
  	        service_builder += "least_hours" -> (data \ "least_hours").asOpt[Int].map (x => x).getOrElse(0)
  	        service_builder += "allow_leave" -> (data \ "allow_leave").asOpt[Int].map (x => x).getOrElse(0)
  	        service_builder += "service_cat" -> (data \ "service_cat").asOpt[Int].map (x => x).getOrElse(0)
  	        
  	        service_builder += "reserve1" -> ""
  	        
  	        _data_connection.getCollection("kidnap") += service_builder.result
  	        
  	        ProfileModule.updateUserProfile(toJson(Map("user_id" -> toJson(owner_id), "is_service_provider" -> toJson(1))))
  	        
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
            
            (data \ "distinct").asOpt[String].map (x => origin += "distinct" -> x).getOrElse(Unit)
            (data \ "address").asOpt[String].map (x => origin += "address" -> x).getOrElse(Unit)
            (data \ "adjust_address").asOpt[String].map (x => origin += "adjust_address" -> x).getOrElse(Unit)
            
            (data \ "age_boundary").asOpt[JsValue].map { boundary => 
                val age_boundary = MongoDBObject.newBuilder
                age_boundary += "lsl" -> (boundary \ "lsl").asOpt[Int].map (x => x).getOrElse(3)
                age_boundary += "usl" -> (boundary \ "usl").asOpt[Int].map (x => x).getOrElse(11)
                origin += "age_boundary" -> age_boundary.result
            }.getOrElse(Unit)

            (data \ "least_hours").asOpt[Int].map (x => origin += "least_hours" -> x.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "allow_leave").asOpt[Int].map (x => origin += "allow_leave" -> x.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "service_cat").asOpt[Int].map (x => origin += "service_cat" -> x.asInstanceOf[Number]).getOrElse(Unit)
            
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
  	               "age_boundary" -> toJson(Map("lsl" -> toJson(x.getAs[MongoDBObject]("age_boundary").get.getAs[Number]("lsl").get.floatValue),
  	                                        "usl" -> toJson(x.getAs[MongoDBObject]("age_boundary").get.getAs[Number]("usl").get.floatValue))),
  	               "cans" -> toJson(x.getAs[Number]("cans").get.longValue),
  	               "facility" -> toJson(x.getAs[Number]("facility").get.longValue),
  	               "distinct" -> toJson(x.getAs[String]("distinct").map(x => x).getOrElse("")),
  	               "address" -> toJson(x.getAs[String]("address").get),
  	               "least_hours" -> toJson(x.getAs[Number]("least_hours").map (y => y.intValue).getOrElse(0)),
  	               "allow_leave" -> toJson(x.getAs[Number]("allow_leave").map (y => y.intValue).getOrElse(1)),
  	               "service_cat" -> toJson(x.getAs[Number]("service_cat").map (y => y.intValue).getOrElse(0)),
  	               "adjust_address" -> toJson(x.getAs[String]("adjust_address").map (y => y).getOrElse("")),
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
  	
  	def queryMultipleService(data : JsValue) : JsValue = 
  	    try {
  	        val lst = (data \ "lst").asOpt[List[String]].map (x => x).getOrElse(throw new Exception)
  	        
  	        def conditionsAcc(id : String, o : Option[DBObject]) : Option[DBObject] = o match {
  	              case None => Some("service_id" $eq id)
  	              case Some(x) => Some($or(x, "service_id" $eq id))
  	            }
  	        
  	        def conditions(l : List[String], o : Option[DBObject]) : Option[DBObject] = l match {
  	          case Nil => o
  	          case head :: tail => conditions(tail, conditionsAcc(head, o))
  	        }
  	        
  	        val reVal = conditions(lst, None) match {
  	          case None => toJson(List[String]())
  	          case Some(x) => toJson((from db() in "kidnap" where x select(DB2JsValue(_))).toList)
  	        }
  
  	        toJson(Map("status" -> toJson("ok"), "result" -> reVal))
  	        
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
}