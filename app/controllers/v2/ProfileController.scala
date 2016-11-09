package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import module.profile.v2.ProfileMessages._

object ProfileController extends Controller {
	def updateUserProfile = Action (request => requestArgsV2(request)(jv => MessageRoutes(msg_UpdateProfile(jv) :: Nil, None)))
	def queryUserProfile = Action (request => requestArgsV2(request)(jv => MessageRoutes(msg_QueryProfile(jv) :: Nil, None)))
}