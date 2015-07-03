package controllers

import play.api.mvc._

import module.notification.apnsNotification

import controllers.common.requestArgsQuery.{requestArgs}

object apnsNotificationController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def registerUserDevices = Action (request => requestArgs(request)(apnsNotification.registerUserDevices))
//	def notificationAll = Action (request => requestArgs(request)(apnsNotification.createSubGroup))
}