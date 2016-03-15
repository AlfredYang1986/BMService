package controllers

import play.api.mvc._

import module.usersearch.UserSearchModule

import controllers.common.requestArgsQuery.{requestArgs, requestArgsWithAuthCheck}

object userSearchController extends Controller {
	def queryRecommandUsers = Action (request => requestArgsWithAuthCheck(request)(UserSearchModule.queryRecommandUsers))
	def queryUsersWithRoleTag = Action (request => requestArgs(request)(UserSearchModule.queryUsersWithRoleTag))
	def queryUsersPosts = Action (request => requestArgsWithAuthCheck(request)(UserSearchModule.queryUsersPosts))
	def queryUsersWithScreenName = Action (request => requestArgsWithAuthCheck(request)(UserSearchModule.queryUsersWithScreenName))
	def queryRecommandUsersWithRoleTag = Action (request => requestArgsWithAuthCheck(request)(UserSearchModule.queryRecommandUsersWithRoleTag))
	def queryUserScreenWithId = Action (request => requestArgs(request)(UserSearchModule.queryUserScreenWithId))
}