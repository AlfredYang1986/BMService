package controllers

import play.api.mvc._

import module.login.LoginModule

import controllers.common.requestArgsQuery.{requestArgs, requestArgsWithAuthCheck}

object loginController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}
    
	def authUpdateDetails = Action (request => requestArgsWithAuthCheck(request)(LoginModule.authUpdateDetails))
	def authWithPhone = Action (request => requestArgs(request)(LoginModule.authWithPhone))
	def authConfirm = Action  (request => requestArgs(request)(LoginModule.authComfirm))
	def authWithThird = Action (request => requestArgs(request)(LoginModule.authWithThird))
	def connectWithThird = Action (request => requestArgs(request)(LoginModule.connectWithThird))
	def authCreateUserWithPhone = Action (request => requestArgs(request)(LoginModule.authCreateUserWithPhone))
	def authWithPwd = Action (request => requestArgs(request)(LoginModule.authWithPwd))
	def logout = Action (request => requestArgsWithAuthCheck(request)(LoginModule.logout))
	def offline = Action (request => requestArgsWithAuthCheck(request)(LoginModule.userOffline))
	def online = Action (request => requestArgsWithAuthCheck(request)(LoginModule.userOnline))
	def userLstInSystem = Action (request => requestArgsWithAuthCheck(request)(LoginModule.userLstInSystem))
	
	def pingTest = Action (request => requestArgsWithAuthCheck(request)(LoginModule.pingTest))
}