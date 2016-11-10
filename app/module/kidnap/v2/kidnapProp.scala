package module.kidnap.v2

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
        case update(data, origin) => sender ! kidnapModule.updateKidnapServiceImpl(data, origin)
        case publish(data, origin) => sender ! kidnapModule.publishKidnapServiceImpl(data, origin)
        case revert(data, origin) => sender ! kidnapModule.revertKidnapServiceImpl(data, origin)
    }
}

case class kidnapProp(val kidnap : ActorRef) {
  	implicit val timeout = Timeout(2 second)
  	def push(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = Await.result((kidnap ? module.kidnap.v2.push(data, origin)).mapTo[(Option[Map[String, JsValue]], Option[JsValue])], timeout.duration)
  	def pop(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = Await.result((kidnap ? module.kidnap.v2.pop(data, origin)).mapTo[(Option[Map[String, JsValue]], Option[JsValue])], timeout.duration)  
  	def update(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = Await.result((kidnap ? module.kidnap.v2.update(data, origin)).mapTo[(Option[Map[String, JsValue]], Option[JsValue])], timeout.duration)  
  	def publish(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = Await.result((kidnap ? module.kidnap.v2.publish(data, origin)).mapTo[(Option[Map[String, JsValue]], Option[JsValue])], timeout.duration)  
  	def revert(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = Await.result((kidnap ? module.kidnap.v2.revert(data, origin)).mapTo[(Option[Map[String, JsValue]], Option[JsValue])], timeout.duration)  
}

class kidnapNoneProp(kidnap : ActorRef) extends kidnapProp(kidnap) {
    override def pop(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
    override def update(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
    override def publish(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
    override def revert(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
}

class kidnapOfflineProp(kidnap : ActorRef) extends kidnapProp(kidnap) {
  	override def push(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
    override def revert(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
}

class kidnapRemovedProp(kidnap : ActorRef) extends kidnapProp(kidnap) {
  	override def push(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
  	override def pop(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
  	override def update(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
  	override def publish(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
  	override def revert(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
}

class kidnapOnlineProp(kidnap : ActorRef) extends kidnapProp(kidnap) {
  	override def push(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
    override def pop(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
    override def update(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
    override def publish(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = (None, Some(ErrorCode.errorToJson("not allowed")))
}

object kidnapProp {
    def status2Handler(status : Int) : kidnapProp = {
        status match {
            case kidnapServiceStatus.none.t => new kidnapNoneProp(kidnapCenter.get)
            case kidnapServiceStatus.offine.t => new kidnapOfflineProp(kidnapCenter.get)
            case kidnapServiceStatus.online.t => new kidnapOnlineProp(kidnapCenter.get)
            case kidnapServiceStatus.removed.t => new kidnapRemovedProp(kidnapCenter.get)
            case _ => ???
        }
    }
  
    var kidnapCenter : Option[ActorRef] = None 
    def apply(status : Int)(implicit app: Application) : kidnapProp = kidnapCenter.map (x => status2Handler(status)).getOrElse {
      		kidnapCenter = Some(Akka.system(app).actorOf(Props[kidnapActor]))
      		status2Handler(status)
      	}
}