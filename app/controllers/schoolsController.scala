package controllers

import play.api.mvc._

import module.profile.SchoolModule

import controllers.common.requestArgsQuery.{requestArgs}

object schoolsController extends Controller {
	def queryAllSchools = Action (request => requestArgs(request)(SchoolModule.queryAllSchools))
	def addOneSchool = Action (request => requestArgs(request)(SchoolModule.addOneSchool))
}