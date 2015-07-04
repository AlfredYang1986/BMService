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
		
		val actor = Akka.system.actorOf(Props(new apnsActor))
		Akka.system.scheduler.schedule(30.seconds, 24.hours, actor, notificationAll) 
	}
	
	override def onStop(application: play.api.Application) = {
		println("application stoped")
	
		apnsNotification.service.stop
	}
}