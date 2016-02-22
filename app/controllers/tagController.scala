package controllers

import play.api.mvc._

import module.tags.TagModule

import controllers.common.requestArgsQuery.{requestArgs}

object tagController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def queryContentsWithTag = Action (request => requestArgs(request)(TagModule.queryContentsWithTag))
	
	def queryRecommandTags = Action (request => requestArgs(request)(TagModule.queryRecommandTags))
	def queryTagPreViewWithTagName = Action (request => requestArgs(request)(TagModule.queryTagPreViewWithTagName))    // preview with post
	def queryFoundSearchTagData = Action (request => requestArgs(request)(TagModule.queryFoundSearchTagData))
	def queryTagSearchWithInput = Action (request => requestArgs(request)(TagModule.queryTagSearchWithInput))          // only query tags
}