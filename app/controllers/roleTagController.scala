package controllers

import play.api.mvc._

import module.profile.RoleTagModule

import controllers.common.requestArgsQuery.{requestArgs, requestArgsWithAuthCheck}

object roleTagController extends Controller {
	def queryAllRoleTags = Action (request => requestArgs(request)(RoleTagModule.queryAllRoleTags))
	def addRoleTag = Action (request => requestArgsWithAuthCheck(request)(RoleTagModule.addRoleTags))
	def queryRecommandRoleTags = Action (request => requestArgs(request)(RoleTagModule.queryAllRoleTags))
	def queryRoleTagPreViewWithRoleTag = Action (request => requestArgsWithAuthCheck(request)(RoleTagModule.queryRoleTagPreViewWithRoleTag))
}