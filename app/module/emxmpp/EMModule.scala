package module.emxmpp

import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson

import EMMessages._
import dongdamessages.MessageDefines
import pattern.ModuleTrait
import util.errorcode.ErrorCode

import module.common.http.HTTP

import com.mongodb.casbah.Imports._
import util.dao.from
import util.dao._data_connection

import java.util.Date
import module.notification.DDNRegisterUser

import play.api.libs.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._

import module.notification.DDNActor
import akka.actor.Props
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.Future
import scala.concurrent.Await

object EMModule extends ModuleTrait {
	val ddn = Akka.system(play.api.Play.current).actorOf(Props[DDNActor])
	implicit val timeout = Timeout(3 second)
	lazy val dongda_common_password = "PassW0rd"
	
	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case msg_RegisterEMUser(data) => registerEMUser(data)(pr)
		case _ => ???
	}  

	def registerEMUser(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = { 
		try {
			val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(pr match {
				case None => throw new Exception("wrong input")	
				case Some(m) => m.get("user_id").map (x => x.asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))).getOrElse(throw new Exception("wrong input"))
			})
			(from db() in "user_profile" where ("user_id" -> user_id) select (x => x)).toList match {
				case head :: Nil => head.getAs[Number]("isEMRegister").map (x => x).getOrElse(0) match {
					case 0 => {
						val result = Await.result((ddn ? DDNRegisterUser ("username" -> toJson(user_id), "password" -> toJson(dongda_common_password), "nickname" -> toJson(user_id))).mapTo[JsValue], timeout.duration)
						(result \ "error").asOpt[String] match {
							case None => {
								head += "isEMRegister" -> 1.asInstanceOf[Number]
								_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), head)
								(pr, None)
							}
							case Some("user existing") => (pr, None)
						}
					}
					case 1 => (pr, None)
				}
				case _ => throw new Exception("user not existing")
			}
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
}