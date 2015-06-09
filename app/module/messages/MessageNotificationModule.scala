package module.messages

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{ toJson }
import play.api.libs.json.{ JsValue, JsObject, JsString, JsArray }
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef

import module.notification._

// messages
case class register(user_id: String) 												// message for register a user
case class unRegister(user_id: String)												// message for quit the chat service
case class channelCreated(user_id: String, channel: Concurrent.Channel[JsValue])	// message for channel created
case class pushNotification(user_id: String, text: String)							// message for push messages (only for test)
case class receiveNotification2(data: JsValue)										// receive message for chat
case class pushNotification2(data: JsValue)											// push message notification for chat
case class registerCallback(user_id: String)										// message for register callback

case class Connected(enumerator:Enumerator[JsValue])								// connect success
case class CannotConnect(msg: String)												// connect not success

class ChatNotifycationCenter extends Actor {

//	var registration_list = Map.empty[String,  Option[Concurrent.Channel[JsValue]]]
	
//	var channel : Option[Concurrent.Channel[JsValue]] = null
	def receive = {
	  case register(user_id) =>
		  // Create an Enumerator to write to this socket
		  val enumerator =  Concurrent.unicast[JsValue] { x => 
		    	self ! channelCreated(user_id, x)
		  }
		
		  if (module.notification.notifyMapping.isUserRegister(user_id)) {
//		  if(registration_list.contains(user_id)) {
			  sender ! CannotConnect("This username is already used")
		  } else {
			  sender ! Connected(enumerator)
		  }    
	  
	  case channelCreated(user_id, channel) => 
	  	  module.notification.notifyMapping.connectAnUser(user_id, channel)
//	    registration_list = registration_list + (user_id -> Some(channel))
	  
	  case unRegister(user_id) => 
	  	  module.notification.notifyMapping.disconnectAnUser(user_id)
//		  registration_list = registration_list - user_id
	  
	  case pushNotification(user_id, text) => Unit
//		  notifyImpl(user_id, text)

	  case receiveNotification2(data) => 
		  (data \ "method").asOpt[String].get match {
		    case "message" => messageModule.sendMessage(data)
		    case "joinTmpGroup" => notifyMapping.userJoinTmpGroup((data \ "user_id").asOpt[String].get, (data \ "receiver").asOpt[String].get)
		    case "unJoinTmpGroup" => notifyMapping.unJoinTmpGroup((data \ "user_id").asOpt[String].get, (data \ "receiver").asOpt[String].get)
		    case _ => ???
		  }
		  
	  case pushNotification2(data) => 
		  notifyMapping.notify2Client(data)
	}
}

class MessageNotificationModule(defaultNotificationCenter : ActorRef) { //(implicit app: Application) {
	
//	lazy val defaultNotificationCenter = Akka.system.actorOf(Props[ChatNotifycationCenter])
	
	implicit val timeout = Timeout(1 second)
	
	def connectWs(user_id : String) : WebSocket[JsValue] = WebSocket.async[JsValue]{ request =>
      	(defaultNotificationCenter ? register(user_id)).map {

        case Connected(enumerator:Enumerator[JsValue]) =>
        	val iteratee = Iteratee.foreach[JsValue] { event =>
        	defaultNotificationCenter ! receiveNotification2(event)
          }.map { _ =>
            defaultNotificationCenter ! unRegister(user_id)
          }
          (iteratee, enumerator)

        case CannotConnect(error) =>
          // Connection error

          // A finished Iteratee sending EOF
          val iteratee = Done[JsValue, Unit]((), Input.EOF)

          // Send an error and close the socket
          val enumerator = Enumerator[JsValue](JsObject(Seq("error" -> JsString(error)))).andThen(Enumerator.enumInput(Input.EOF))

          (iteratee, enumerator)
      }
    }
}

object MessageNotificationModule {
  
	var defaultNotificationCenter : Option[ActorRef] = None //Akka.system.actorOf(Props[ChatNotifycationCenter])
	def apply()(implicit app: Application) : MessageNotificationModule = defaultNotificationCenter.map(nc =>
		new MessageNotificationModule(nc)).getOrElse {
	  		defaultNotificationCenter = Some(Akka.system(app).actorOf(Props[ChatNotifycationCenter]))
	  		new MessageNotificationModule(defaultNotificationCenter.get)
	  	}
}