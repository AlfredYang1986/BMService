package module.kidnap

import play.api._
import play.api.libs.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef
import scala.concurrent.Await

import util.errorcode.ErrorCode

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import com.mongodb.casbah.Imports._

object kidnapServiceStatus {
    case object none extends kidnapServiceStatusDefines(0, "none")
    case object offine extends kidnapServiceStatusDefines(1, "offine")
    case object online extends kidnapServiceStatusDefines(2, "online")
    case object removed extends kidnapServiceStatusDefines(3, "removed")
}

sealed abstract class kidnapServiceStatusDefines(val t : Int, val des : String)

case class push(data : JsValue, origin : MongoDBObject)
case class pop(data : JsValue, origin : MongoDBObject)
case class update(data : JsValue, origin : MongoDBObject)
case class publish(data : JsValue, origin : MongoDBObject)
case class revert(data : JsValue, origin : MongoDBObject)

class kidnapActor extends Actor {
  	implicit val timeout = Timeout(2 second)
 
  	def receive = {
        case push(data, origin) => sender ! kidnapModule.pushKidnapServiceImpl(data, origin)
        case pop(data, origin) => sender ! kidnapModule.popKidnapServiceImpl(data, origin)
          
        }
        
        case update(data, origin) => {
          
        }
        
        case publish(data, origin) => {
          
        }
        
        case revert(data, origin) => {
          
        }
    }
}

case class kidnapProp(val kidnap : ActorRef) {
  	implicit val timeout = Timeout(2 second)
  	def push(data : JsValue, origin : MongoDBObject) : JsValue = Await.result((kidnap ? push(data, origin)).mapTo[JsValue], timeout.duration)  
  	def pop(data : JsValue, origin : MongoDBObject) : JsValue = Await.result((kidnap ? pop(data, origin)).mapTo[JsValue], timeout.duration)  
  	def update(data : JsValue, origin : MongoDBObject) : JsValue = Await.result((kidnap ? update(data, origin)).mapTo[JsValue], timeout.duration)  
  	def publish(data : JsValue, origin : MongoDBObject) : JsValue = Await.result((kidnap ? publish(data, origin)).mapTo[JsValue], timeout.duration)  
  	def revert(data : JsValue, origin : MongoDBObject) : JsValue = Await.result((kidnap ? revert(data, origin)).mapTo[JsValue], timeout.duration)  
}

class kidnapNoneProp(val kidnap : ActorRef) extends kidnapProp(kidnap) {
    def pop(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
    def update(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
    def publish(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
    def revert(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
}

class kidnapOfflineProp(val kidnap : ActorRef) extends kidnapProp(kidnap) {
  	def push(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
    def revert(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
}

class kidnapRemovedProp(val kidnap : ActorRef) extends kidnapProp(kidnap) {
  	def push(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
  	def pop(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
  	def update(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
  	def publish(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
  	def revert(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
}

class kidnapOnlineProp(val kidnap : ActorRef) extends kidnapProp(kidnap) {
  	def push(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
    def pop(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
    def update(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
    def publish(data : JsValue, origin : MongoDBObject) : JsValue = ErrorCode.errorToJson("not allowed")
}

object kidnapProp {
    def status2Handler(status : Int) : kidnapProp = {
        status match {
            case kidnapServiceStatus.none.t => new kidnapNoneProp(kidnapCenter.get)
            case kidnapServiceStatus.offine.t => new kidnapOfflineProp(kidnapCenter.get)
            case kidnapServiceStatus.online.t => new kidnapOnlineProp(kidnapCenter.get)
            case _ => ???
        }
    }
  
    var kidnapCenter : Option[ActorRef] = None 
    def apply(status : Int)(implicit app: Application) : kidnapProp = kidnapCenter.map (x => status2Handler(status)).getOrElse {
      		kidnapCenter = Some(Akka.system(app).actorOf(Props[kidnapActor]))
      		status2Handler(status)
      	}
}