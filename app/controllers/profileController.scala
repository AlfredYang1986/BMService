package controllers

import play.api.mvc._
import module.profile.ProfileModule
import module.profile.DetailProfileModule
import controllers.common.requestArgsQuery.{requestArgs}

object profileController extends Controller {
	def like = Action (request => requestArgs(request)(ProfileModule.like))
	def userProfile = Action (request => requestArgs(request)(ProfileModule.userProfile))
	def updateProfile = Action (request => requestArgs(request)(ProfileModule.updateUserProfile))
	def multipleUserProfile = Action (request => requestArgs(request)(ProfileModule.multipleUserProfile))
	
	def queryDetailDescription = Action (request => requestArgs(request)(DetailProfileModule.queryDetailDescription))
	def createAndUpdateDettailDescription = Action (request => requestArgs(request)(DetailProfileModule.createAndUpdateDetailDescription))
}