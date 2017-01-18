package module.auth

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

abstract class msg_AuthCommand extends CommonMessage

object AuthMessage {
	case class msg_AuthPhoneCode(data : JsValue) extends msg_AuthCommand
	case class msg_AuthThird(data : JsValue) extends msg_AuthCommand
	case class msg_AuthSignOut(data : JsValue) extends msg_AuthCommand
	case class msg_AuthQuery(data : JsValue) extends msg_AuthCommand
	case class msg_AuthCheck(data : JsValue) extends msg_AuthCommand
}
