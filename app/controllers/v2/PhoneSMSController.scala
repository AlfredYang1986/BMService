package controllers.v2

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import module.phonecode.PhoneCodeMessages._
import pattern.LogMessage.msg_log
import pattern.ResultMessage.msg_CommonResultMessage
import play.api.libs.json.Json.toJson

object PhoneSMSController extends Controller {
	def sendSMSCode = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
		    import pattern.LogMessage.common_log
    		MessageRoutes(msg_log(toJson(Map("method" -> toJson("send sms code"))), jv)
                :: msg_SendSMSCode(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def checkSMSCode = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
            import pattern.LogMessage.common_log
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("check sms code"))), jv)
			    :: msg_CheckSMSCode(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
}