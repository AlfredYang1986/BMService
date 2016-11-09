package module.phonecode

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

abstract class msg_PhoneCodeCommand extends CommonMessage

object PhoneCodeMessages {
	case class msg_SendSMSCode(data : JsValue) extends msg_PhoneCodeCommand
	case class msg_CheckSMSCode(data : JsValue) extends msg_PhoneCodeCommand
}