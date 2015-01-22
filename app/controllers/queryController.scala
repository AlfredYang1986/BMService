package controllers

import play.api.mvc._

import module.query.QueryModule

import controllers.common.requestArgsQuery.{requestArgs}

object queryController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}
	
	def queryHomeContent = Action ( request => requestArgs(request)(QueryModule.queryHomeContent))
	def queryComments = Action ( request => requestArgs(request)(QueryModule.queryComments))
	def queryLikes = Action ( request => requestArgs(request)(QueryModule.queryLikes))

	def downloadFile(name : String) = Action ( Ok(QueryModule.downloadFile(name)).as("image/png"))
}