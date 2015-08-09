import akka.actor.{Actor, Props}
import play.api.libs.concurrent.Akka
import play.api.GlobalSettings
import play.api.templates.Html
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import module.notification._

object Global extends GlobalSettings {
	
	override def onStart(application: play.api.Application)  = {
		import scala.concurrent.duration._
		import play.api.Play.current
		println("application started")
		
		val actor = Akka.system(application).actorOf(Props[apnsActor])
		Akka.system(application).scheduler.schedule(30.seconds, 24.hours, actor, apnsNotificationAll) 

		val ddn = Akka.system(application).actorOf(Props[DDNActor])
		Akka.system(application).scheduler.schedule(0.seconds, 23.hours + 50.minutes, ddn, DDNInit) 
	}
	
	override def onStop(application: play.api.Application) = {
		println("application stoped")
	
		apnsNotification.service.stop
	}
}