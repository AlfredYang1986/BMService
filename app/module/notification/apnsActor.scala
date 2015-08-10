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
case object apnsNotificationAll
case class 	apnsNotifyUsers(message : String, to : String, action : Int)

class apnsActor extends Actor {
	
	def receive = {
	  case module.notification.apnsNotificationAll => apnsNotification.notificationAll
	  case notify : apnsNotifyUsers => apnsNotification.nofity(notify.message, notify.to, notify.action)
	  case _ => 
	}
}

