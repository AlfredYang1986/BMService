package pattern

import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.ActorContext
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import dongdamessages._
import module.auth.AuthModule
import module.auth.msg_AuthCommand
import module.emxmpp.EMModule
import module.emxmpp.msg_EMMessageCommand
import module.kidnap.v2.kidnapModule
import module.kidnap.v2.msg_KidnapServiceCommand
import module.order.v2.msg_OrderCommand
import module.order.v2.orderModule
import module.phonecode.PhoneCodeModule
import module.phonecode.msg_PhoneCodeCommand
import module.profile.v2.ProfileModule
import module.profile.v2.msg_ProfileCommand
import module.test.msg_TestCommand
import module.test.testModule
import play.api.Application
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson

object PipeFilterActor {
	def prop(originSender : ActorRef, msr : MessageRoutes) : Props = {
		Props(new PipeFilterActor(originSender, msr))
	}
}

class PipeFilterActor(originSender : ActorRef, msr : MessageRoutes) extends Actor with ActorLogging {
	
	def dispatchImpl(cmd : CommonMessage, module : ModuleTrait) = {
		tmp = Some(true)
		module.dispatchMsg(cmd)(rst) match {
			case (_, Some(err)) => {
				originSender ! error(err)
				cancelActor					
			}
			case (Some(r), _) => {
				rst = Some(r) 
			}
			case _ => println("never go here")
		}
		rstReturn
	}
	
	var tmp : Option[Boolean] = None
	var rst : Option[Map[String, JsValue]] = msr.rst
	var next : ActorRef = null
	def receive = {
		case cmd : msg_AuthCommand => dispatchImpl(cmd, AuthModule)
		case cmd : msg_PhoneCodeCommand => dispatchImpl(cmd, PhoneCodeModule)
		case cmd : msg_ProfileCommand => dispatchImpl(cmd, ProfileModule)
		case cmd : msg_EMMessageCommand => dispatchImpl(cmd, EMModule)  
		case cmd : msg_KidnapServiceCommand => dispatchImpl(cmd, kidnapModule)
		case cmd : msg_OrderCommand => dispatchImpl(cmd, orderModule)
		case cmd : msg_ResultCommand => dispatchImpl(cmd, ResultModule)
		case cmd : msg_TestCommand => dispatchImpl(cmd, testModule)
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
						next = context.actorOf(PipeFilterActor.prop(originSender, MessageRoutes(tail, rst)), "pipe")
						next ! head
					}
					case _ => println("msr error")
				}
				cancelActor
			case _ => Unit
		}}
		case _ => println("never go here"); Unit
	}
	
	def cancelActor = {
		timeOutSchdule.cancel
//		context.stop(self)
	}
}