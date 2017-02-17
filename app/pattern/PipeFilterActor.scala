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
import module.kidnap.v2.kidnapCollectionModule
import module.kidnap.v2.msg_KidnapServiceCollectionCommand
import module.order.v2.msg_OrderCommand
import module.order.v2.orderModule
import module.phonecode.PhoneCodeModule
import module.phonecode.msg_PhoneCodeCommand
import module.profile.v2.ProfileModule
import module.profile.v2.msg_ProfileCommand
import module.order.v2.msg_OrderCommentsCommand
import module.order.v2.orderCommentsModule
import module.realname.msg_RealNameCommand
import module.realname.RealNameModule
import module.timemanager.v3.msg_TMCommand
import module.timemanager.v3.TMModule


import module.kidnap.v2.kidnapModule
import module.kidnap.v2.msg_KidnapServiceCommand
import module.kidnap.v3.kidnapModule
import module.kidnap.v3.msg_KidnapServiceCommand

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
//				println(r)
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
		case cmd : module.kidnap.v2.msg_KidnapServiceCommand => dispatchImpl(cmd, module.kidnap.v2.kidnapModule)
        case cmd : module.kidnap.v3.msg_KidnapServiceCommand => dispatchImpl(cmd, module.kidnap.v3.kidnapModule)
		case cmd : module.kidnap.v2.msg_KidnapServiceCollectionCommand => dispatchImpl(cmd, module.kidnap.v2.kidnapCollectionModule)
		case cmd : module.kidnap.v3.msg_KidnapServiceCollectionCommand => dispatchImpl(cmd, module.kidnap.v3.kidnapCollectionModule)
		case cmd : msg_OrderCommand => dispatchImpl(cmd, orderModule)
		case cmd : msg_OrderCommentsCommand => dispatchImpl(cmd, orderCommentsModule)
		case cmd : msg_ResultCommand => dispatchImpl(cmd, ResultModule)
        case cmd : msg_LogCommand => dispatchImpl(cmd, LogModule)
        case cmd : msg_RealNameCommand => dispatchImpl(cmd, RealNameModule)
        case cmd : msg_TMCommand => dispatchImpl(cmd, TMModule)
		case cmd : ParallelMessage => {
		    cancelActor
			next = context.actorOf(ScatterGatherActor.prop(originSender, msr), "scat")
			next ! cmd
		}
		case timeout() => {
			originSender ! new timeout
			cancelActor
		}
	 	case x : AnyRef => println(x); ???
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
						head match {
							case p : ParallelMessage => {
								next = context.actorOf(ScatterGatherActor.prop(originSender, MessageRoutes(tail, rst)), "scat")
								next ! p
							}
							case c : CommonMessage => {
								next = context.actorOf(PipeFilterActor.prop(originSender, MessageRoutes(tail, rst)), "pipe")
								next ! c
							}
						}
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