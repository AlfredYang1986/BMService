package controllers

import play.api.mvc._

import module.usersearch.UserSearchModule

import controllers.common.requestArgsQuery.{requestArgs}

object userSearchController extends Controller {
	def queryRecommandUsers = Action (request => requestArgs(request)(UserSearchModule.queryRecommandUsers))
	def queryUsersWithRoleTag = Action (request => requestArgs(request)(UserSearchModule.queryUsersWithRoleTag))
	def queryUsersPosts = Action (request => requestArgs(request)(UserSearchModule.queryUsersPosts))
	def queryUsersWithScreenName = Action (request => requestArgs(request)(UserSearchModule.queryUsersWithScreenName))
	def queryRecommandUsersWithRoleTag = Action (request => requestArgs(request)(UserSearchModule.queryRecommandUsersWithRoleTag))
	def queryUserScreenWithId = Action (request => requestArgs(request)(UserSearchModule.queryUserScreenWithId))
}