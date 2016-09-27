package module.login

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import module.sms._
import module.relationship._
import module.profile.ProfileModule
import module.notification._

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef

object realNameStatus {  
    case object pushed extends RealNameAuthDefines(0, "status base")
    case object approved extends RealNameAuthDefines(1, "approved")
    case object rejected extends RealNameAuthDefines(2, "rejected")
}

sealed abstract class RealNameAuthDefines(val t : Int, val des : String)

object RealNameModule {
    def pushRealName(data : JsValue) : JsValue = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val real_name = (data \ "real_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val sociel_id = (data \ "sociel_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
              case head :: Nil => {
                  null
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)      
        }
    }
    
    def approveRealName(data : JsValue) : JsValue = {
        null 
    }
     
    def rejectRealName(data : JsValue) : JsValue = {
        null
    }
}