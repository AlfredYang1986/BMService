package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import module.profile.v2.ProfileMessages._
import pattern.ResultMessage.msg_CommonResultMessage
import module.auth.AuthMessage.msg_AuthQuery

object ProfileController extends Controller {
	def updateUserProfile = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_UpdateProfile(jv) :: msg_CommonResultMessage() ::Nil, None)
		})
	def queryUserProfile = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_QueryProfile(jv) :: msg_AuthQuery(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
}