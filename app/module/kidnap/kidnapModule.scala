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
                            case Nil => null
                            case head :: Nil => (head.getAs[Number]("status").get.intValue, head)
                            case _ => null
                          }
          case None => null
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
  	        
  	        null
  	      
  	    } catch {
  	      case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
  	    }
  	    
  	}
  	
  	def popKidnapService(data : JsValue) : JsValue = {
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).pop(data, origin)
  	}
  	
  	def updateKidnapService(data : JsValue) : JsValue = { 
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).update(data, origin)
  	}
  	
  	def publishKidnapService(data : JsValue) : JsValue = {
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).publish(data, origin)
  	}
  	
  	def revertKidnapService(data : JsValue) : JsValue = { 
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).revert(data, origin)
  	}
}