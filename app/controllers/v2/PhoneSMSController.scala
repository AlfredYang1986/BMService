package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import module.phonecode.PhoneCodeMessages._

object PhoneSMSController extends Controller {
	def sendSMSCode = Action (request => requestArgsV2(request)(jv => MessageRoutes(msg_SendSMSCode(jv) :: Nil, None)))
	def checkSMSCode = Action (request => requestArgsV2(request)(jv => MessageRoutes(msg_CheckSMSCode(jv) :: Nil, None)))
}