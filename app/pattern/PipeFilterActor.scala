package pattern

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import akka.actor.ActorLogging
import akka.actor.ActorRef
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

object PipeFilterActor {
	def apply(originSender : ActorRef, msr : MessageRoutes) = {
		ActorSystem("sys").actorOf(Props(new PipeFilterActor(originSender, msr)), "pipe")
	}
}

class PipeFilterActor(originSender : ActorRef, msr : MessageRoutes) extends Actor with ActorLogging {
	var tmp : Option[Boolean] = None
	var rst : Option[Map[String, JsValue]] = msr.rst
	def receive = {
		case cmd : msg_AuthCommand => {
	 	 	tmp = Some(true)
			AuthModule.dispatchMsg(cmd)(rst) match {
				case (_, Some(err)) => {
					originSender ! error(err)
					cancelActor					
				}
				case (Some(r), _) => rst = Some(r) 
			}
			rstReturn
		}
		case cmd : msg_PhoneCodeCommand => {
			tmp = Some(true)
			PhoneCodeModule.dispatchMsg(cmd)(rst) match {
				case (_, Some(err)) => {
					originSender ! error(err)
					cancelActor
				}
				case (Some(r), _) => rst = Some(r)
			}
			rstReturn
		}
		case cmd : msg_ProfileCommand => {
			tmp = Some(true)
			ProfileModule.dispatchMsg(cmd)(rst) match {
				case (_, Some(err)) => {
					originSender ! error(err)
					cancelActor
				}
				case (Some(r), _) => rst = Some(r)
			}
			rstReturn
		}
		case cmd : msg_EMMessageCommand => {
			tmp = Some(true)
			EMModule.dispatchMsg(cmd)(rst) match {
				case (_, Some(err)) => {
					originSender ! error(err)
					cancelActor
				}
				case (Some(r), _) => rst = Some(r)
			}
			rstReturn
		}
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
						originSender ! result(toJson(Map("status" -> toJson("ok"), "result" -> toJson(r))))
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