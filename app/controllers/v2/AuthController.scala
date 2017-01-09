package controllers.v2

import play.api._
import play.api.mvc._

import play.api.libs.json.Json.toJson
import play.api.libs.json.JsValue

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import module.auth.AuthMessage.{ msg_AuthPhoneCode, msg_AuthThird, msg_AuthSignOut }
import module.phonecode.PhoneCodeMessages.msg_CheckSMSCode
import module.profile.v2.ProfileMessages.msg_UpdateProfile
import module.emxmpp.EMMessages.msg_RegisterEMUser
import pattern.ResultMessage.msg_CommonResultMessage
import pattern.LogMessage.msg_log

object AuthController extends Controller {
	def authWithPhoneCode = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			import pattern.LogMessage.common_log
			MessageRoutes(msg_log(toJson(Map("method" -> toJson("auth with phone code"))), jv)
                :: msg_CheckSMSCode(jv) :: msg_AuthPhoneCode(jv) :: msg_UpdateProfile(jv)
                :: msg_RegisterEMUser(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def authWithThird = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
            import pattern.LogMessage.common_log
			MessageRoutes(msg_log(toJson(Map("method" -> toJson("auth with sns"))), jv)
                :: msg_AuthThird(jv) :: msg_UpdateProfile(jv) :: msg_RegisterEMUser(jv)
                :: msg_CommonResultMessage() :: Nil, None)
		})
	def authSignOut = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
            import pattern.LogMessage.common_log
			MessageRoutes(msg_log(toJson(Map("method" -> toJson("auth sign out"))), jv)
                :: msg_AuthSignOut(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
}