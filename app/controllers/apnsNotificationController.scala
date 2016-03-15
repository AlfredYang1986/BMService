package controllers

import play.api.mvc._

import module.notification.apnsNotification

import controllers.common.requestArgsQuery.{requestArgs, requestArgsWithAuthCheck}

object apnsNotificationController extends Controller {
	def registerUserDevices = Action (request => requestArgsWithAuthCheck(request)(apnsNotification.registerUserDevices))
}