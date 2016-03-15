package controllers

import play.api.mvc._
import module.profile.ProfileModule
import module.profile.DetailProfileModule
import controllers.common.requestArgsQuery.requestArgs
import controllers.common.requestArgsQuery.requestArgsWithAuthCheck

object profileController extends Controller {
	def userProfile = Action (request => requestArgsWithAuthCheck(request)(ProfileModule.userProfile))
	def updateProfile = Action (request => requestArgs(request)(ProfileModule.updateUserProfile))
	def multipleUserProfile = Action (request => requestArgsWithAuthCheck(request)(ProfileModule.multipleUserProfile))
	def recommendUserProfile = Action (request => requestArgsWithAuthCheck(request)(ProfileModule.recommendUserProfile))
	
	def queryDetailDescription = Action (request => requestArgs(request)(DetailProfileModule.queryDetailDescription))
	def createAndUpdateDettailDescription = Action (request => requestArgs(request)(DetailProfileModule.createAndUpdateDetailDescription))
}