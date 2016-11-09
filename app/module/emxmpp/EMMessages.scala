package module.emxmpp

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

abstract class msg_EMMessageCommand extends CommonMessage

object EMMessages {
	case class msg_EMToken(data : JsValue) extends msg_EMMessageCommand
	case class msg_RegisterEMUser(data : JsValue) extends msg_EMMessageCommand
}