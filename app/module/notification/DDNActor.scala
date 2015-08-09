package module.notification

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.actor.Inbox
import scala.concurrent.duration._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

/**
 * messages for start schedule notification
 */
case object DDNInit
case class 	DDNNotifyUsers(val parameters : (String, JsValue)*)

class DDNActor extends Actor {
	
	def receive = {
	  case DDNInit => DDNNotification.getAuthTokenForXMPP
	  case notify : DDNNotifyUsers => {
		  var pm : Map[String, JsValue] = Map.empty
		  for ((key, value) <- notify.parameters) pm += key -> value
		  DDNNotification.nofity(pm)
	  }
	  case _ => 
	}
}