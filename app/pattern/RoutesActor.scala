package pattern

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorLogging
import akka.actor.Props

import play.api.libs.json.Json.toJson

import dongdamessages._
import module.auth.AuthMessage._

class RoutesActor extends Actor with ActorLogging {
	var originSender : ActorRef = null
	var next : ActorRef = null
	def receive = {
		case excute(msr)  => {
			originSender = sender
			msr.lst match {
				case Nil => originSender ! toJson("error")
				case head :: tail => {
					next = context.actorOf(PipeFilterActor.prop(self, MessageRoutes(tail, msr.rst)), "gate")
					next ! head
				}
			}
		}
		case result(rst) => {
			originSender ! rst
			cancelActor
		}
		case error(err) => {
			originSender ! err
			cancelActor
		}
		case dongdamessages.timeout() => {
			originSender ! toJson("timeout")
			cancelActor
		}
		case _ => ???
 	}
	
	def cancelActor = {
		context.stop(self)
	}
}