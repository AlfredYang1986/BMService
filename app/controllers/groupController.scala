package controllers

import play.api.mvc._

import module.groups.GroupModule
import module.groups.GroupModule2

import controllers.common.requestArgsQuery.{requestArgs}

object groupController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def queryGroups = Action (request => requestArgs(request)(GroupModule.queryGroups))
	def createSubGroup = Action (request => requestArgs(request)(GroupModule.createSubGroup))
	
	def createChatGroup = Action (request => requestArgs(request)(GroupModule2.createChatGroup))
	def updateChatGroup = Action (request => requestArgs(request)(GroupModule2.updateChatGroup))
	def joinChatGroup = Action (request => requestArgs(request)(GroupModule2.joinChatGroup))
	def leaveChatGroup = Action (request => requestArgs(request)(GroupModule2.leaveChatGroup))
	def dissmissChatGroup = Action (request => requestArgs(request)(GroupModule2.delectChatGroup))

	def queryChatGroup = Action (request => requestArgs(request)(GroupModule2.queryGroups))
}