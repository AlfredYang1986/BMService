package pattern

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorLogging

import play.api.libs.json.Json.toJson

import dongdamessages._
import module.auth.AuthMessage._

class RoutesActor extends Actor with ActorLogging {
	var originSender : ActorRef = null
	def receive = {
		case excute(msr)  => {
			originSender = sender
			msr.lst match {
				case Nil => originSender ! toJson("error")
				case head :: tail => {
					val handle = PipeFilterActor(self, MessageRoutes(tail, msr.rst))
					handle ! head
				}
			}
		}
		case result(rst) => {
			originSender ! rst
		}
		case error(err) => {
			originSender ! err
		}
		case dongdamessages.timeout() => {
			originSender ! toJson("timeout")
		}
		case _ => ???
 	}
}