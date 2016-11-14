package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import module.auth.AuthMessage.msg_AuthPhoneCode
import module.auth.AuthMessage.msg_AuthThird
import module.auth.AuthMessage.msg_AuthSignOut
import module.phonecode.PhoneCodeMessages.msg_CheckSMSCode
import module.profile.v2.ProfileMessages.msg_UpdateProfile
import module.emxmpp.EMMessages.msg_RegisterEMUser
import pattern.ResultMessage.msg_CommonResultMessage

object AuthController extends Controller {
	def authWithPhoneCode = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_CheckSMSCode(jv) :: msg_AuthPhoneCode(jv) :: msg_UpdateProfile(jv) :: msg_RegisterEMUser(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def authWithThird = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_AuthThird(jv) :: msg_UpdateProfile(jv) :: msg_RegisterEMUser(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def authSignOut = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_AuthSignOut(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
}