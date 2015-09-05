package controllers

import play.api.mvc._

import module.email.EmailModule

import controllers.common.requestArgsQuery.{requestArgs}

object emailController extends Controller {
	def sendEmail = Action (request => requestArgs(request)(EmailModule.sendEmail))
}