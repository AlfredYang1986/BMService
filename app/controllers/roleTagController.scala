package controllers

import play.api.mvc._

import module.profile.RoleTagModule

import controllers.common.requestArgsQuery.{requestArgs}

object roleTagController extends Controller {
	def queryAllRoleTags = Action (request => requestArgs(request)(RoleTagModule.queryAllRoleTags))
	def addRoleTag = Action (request => requestArgs(request)(RoleTagModule.addRoleTags))
}