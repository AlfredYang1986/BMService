package pattern

import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson

import dongdamessages.MessageDefines
import ResultMessage.msg_CommonResultMessage

object ResultModule extends ModuleTrait {
	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case cmd : msg_CommonResultMessage => cmd.func(pr.get)
	}
}