package controllers

import play.api.mvc._

import module.groups.GroupModule

import controllers.common.requestArgsQuery.{requestArgs}

object groupController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def queryGroups = Action (request => requestArgs(request)(GroupModule.queryGroups))
	def createSubGroup = Action (request => requestArgs(request)(GroupModule.createSubGroup))
}