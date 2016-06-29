package controllers

import play.api.mvc._

import module.groups.GroupModule
import module.groups.GroupModule2

import controllers.common.requestArgsQuery.{requestArgs}

object groupController extends Controller {
	def queryGroups = Action (request => requestArgs(request)(GroupModule.queryGroups))
	def createSubGroup = Action (request => requestArgs(request)(GroupModule.createSubGroup))

	/**
	 * create chat room
	 */
//	def createChatRoom = Action (request => requestArgs(request)(GroupModule2.createChatRoom))
//	def updateChatRoom = Action (request => requestArgs(request)(GroupModule2.updateChatRoom))
//	def joinChatRoom = Action (request => requestArgs(request)(GroupModule2.joinChatRoom))
//	def leaveChatRoom = Action (request => requestArgs(request)(GroupModule2.leaveChatRoom))
//	def dissmissChatRoom = Action (request => requestArgs(request)(GroupModule2.delectChatRoom))

	/**
	 * create chat group
	 */
	def createChatGroup = Action (request => requestArgs(request)(GroupModule2.createChatGroup))
	def updateChatGroup = Action (request => requestArgs(request)(GroupModule2.updateChatGroup))
	def joinChatGroup = Action (request => requestArgs(request)(GroupModule2.joinChatGroup))
	def leaveChatGroup = Action (request => requestArgs(request)(GroupModule2.leaveChatGroup))
	def dissmissChatGroup = Action (request => requestArgs(request)(GroupModule2.deleteChatGroup))
	
	def queryChatGroup = Action (request => requestArgs(request)(GroupModule2.queryGroups))
}