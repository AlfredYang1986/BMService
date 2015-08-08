package module.notification

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.actor.Inbox
import scala.concurrent.duration._

/**
 * messages for start schedule notification
 */
case object DDNInit 

class DDNActor extends Actor {
	
	def receive = {
	  case DDNInit => DDNNotification.getAuthTokenForXMPP
	  case _ => 
	}
}