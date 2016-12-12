package pattern

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props
import dongdamessages.MessageRoutes
import scala.concurrent.stm._
import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson

import ParallelMessage.f
import dongdamessages.result

import scala.concurrent.duration._
import dongdamessages.timeout
import dongdamessages.error
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import dongdamessages.CommonMessage
import play.api.libs.json.JsObject

object ScatterGatherActor {
	def prop(originSender : ActorRef, msr : MessageRoutes) : Props = {
		Props(new ScatterGatherActor(originSender, msr))
	}
}

class ScatterGatherActor(originSender : ActorRef, msr : MessageRoutes) extends Actor with ActorLogging {

	var next : ActorRef = null
	var sub_act = Seq[ActorRef]()
	var excepted = 0
	val tmp_result : Ref[List[Map[String, JsValue]]] = Ref(Nil) 
	var f : List[Map[String, JsValue]] => Map[String, JsValue] = null
	var rst : Option[Map[String, JsValue]] = msr.rst
	
	def receive = {
		case ParallelMessage(msgs, merge) => {
			f = merge
			excepted = msgs.length
			msgs.foreach { m => 
				val act = context.actorOf(ScatterGatherStepActor.prop(self, MessageRoutes(m.lst.tail, rst)))
				act ! m.lst.head
				sub_act = sub_act :+ act
			}
		}
		case ParalleMessageSuccess(r) => stepSuccess(r)
		case ParalleMessageFailed(err) => {
			originSender ! error(err)
			cancelActor					
		}
		case result(r) => stepSuccess(r.as[JsObject].value.toMap)
		case err : error => {
			originSender ! err 
			cancelActor
		}
		case timeout() => Unit
		case x : AnyRef => println(s"something messages: $x"); ???
	}
	
	val timeOutSchdule = context.system.scheduler.scheduleOnce(2 second, self, new timeout)

	def rstReturn = {
		msr.lst match {
			case Nil => {
				originSender ! result(toJson(f(tmp_result.single.get)))
			}
			case head :: tail => {
				val rst = Some(f(tmp_result.single.get))
				head match {
					case p : ParallelMessage => {
						next = context.actorOf(ScatterGatherActor.prop(originSender, MessageRoutes(tail, rst)), "scat")
						next ! head
					}
					case c : CommonMessage => {
						next = context.actorOf(PipeFilterActor.prop(originSender, MessageRoutes(tail, rst)), "pipe")
						next ! head
					}
				}
			}
			case _ => println("msr error")
		}
		cancelActor
	}
	
	def cancelActor = {
		timeOutSchdule.cancel
//		context.stop(self)
	}
	
	def stepSuccess(r : Map[String, JsValue]) = {
		atomic { implicit thx => 
		    tmp_result() = tmp_result() :+ r 
		}
		
		if (tmp_result.single.get.length == excepted) 
		    rstReturn
	}
}