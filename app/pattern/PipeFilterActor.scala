package pattern

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.ActorContext
import play.api.Application
import play.api.libs.concurrent.Akka
import dongdamessages._
import play.api.libs.json.JsValue
import play.api.libs.json.Json.{toJson}
import akka.util.Timeout
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import module.auth.{ AuthModule, msg_AuthCommand }
import module.phonecode.{ msg_PhoneCodeCommand, PhoneCodeModule }
import module.profile.v2.{ msg_ProfileCommand, ProfileModule }
import module.emxmpp.{ msg_EMMessageCommand, EMModule }
import module.kidnap.v2.{ msg_KidnapServiceCommand, kidnapModule }
import module.order.v2.{ msg_OrderCommand, orderModule }

object PipeFilterActor {
	def apply(originSender : ActorRef, msr : MessageRoutes)(implicit c : ActorContext) = {
		c.actorOf(PipeFilterActor.prop(originSender, msr), "pipe")
	}
	
	def prop(originSender : ActorRef, msr : MessageRoutes) : Props = Props(new PipeFilterActor(originSender, msr))
}

class PipeFilterActor(originSender : ActorRef, msr : MessageRoutes) extends Actor with ActorLogging {
	
	def dispatchImpl(cmd : CommonMessage, module : ModuleTrait) = {
		tmp = Some(true)
		module.dispatchMsg(cmd)(rst) match {
			case (_, Some(err)) => {
				originSender ! error(err)
				cancelActor					
			}
			case (Some(r), _) => rst = Some(r) 
		}
		rstReturn
	}
	
	var tmp : Option[Boolean] = None
	var rst : Option[Map[String, JsValue]] = msr.rst
	def receive = {
		case cmd : msg_AuthCommand => dispatchImpl(cmd, AuthModule)
		case cmd : msg_PhoneCodeCommand => dispatchImpl(cmd, PhoneCodeModule)
		case cmd : msg_ProfileCommand => dispatchImpl(cmd, ProfileModule)
		case cmd : msg_EMMessageCommand => dispatchImpl(cmd, EMModule)  
		case cmd : msg_KidnapServiceCommand => dispatchImpl(cmd, kidnapModule)
		case cmd : msg_OrderCommand => dispatchImpl(cmd, orderModule)
		case cmd : msg_ResultCommand => dispatchImpl(cmd, ResultModule)
		case timeout() => {
			originSender ! new timeout
			cancelActor
		}
	 	case _ => ???
	}
	
	val timeOutSchdule = context.system.scheduler.scheduleOnce(2 second, self, new timeout)

	def rstReturn = tmp match {
		case Some(_) => { rst match {
			case Some(r) => 
				msr.lst match {
					case Nil => {
						originSender ! result(toJson(r))
					}
					case head :: tail => {
						val handle = PipeFilterActor(originSender, MessageRoutes(tail, rst))
						handle ! head
					}
				}
				cancelActor
			case _ => Unit
		}}
		case _ => Unit
	}
	
	def cancelActor = {
		timeOutSchdule.cancel
		context.stop(self)
	}
}