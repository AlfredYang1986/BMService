package controllers

import play.api.mvc._

import module.tags.TagModule

import controllers.common.requestArgsQuery.{requestArgs}

object tagController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def queryContentsWithTag = Action (request => requestArgs(request)(TagModule.queryContentsWithTag))
}