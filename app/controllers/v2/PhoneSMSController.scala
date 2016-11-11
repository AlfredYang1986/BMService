package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import module.phonecode.PhoneCodeMessages._
import pattern.ResultMessage.msg_CommonResultMessage

object PhoneSMSController extends Controller {
	def sendSMSCode = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_SendSMSCode(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def checkSMSCode = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_CheckSMSCode(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
}