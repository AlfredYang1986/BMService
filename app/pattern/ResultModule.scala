package pattern

import java.util.Date

import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson
import dongdamessages.MessageDefines
import ResultMessage.{msg_CommonResultMessage, msg_PlusDateToResultMessage}
import util.errorcode.ErrorCode

object ResultModule extends ModuleTrait {
	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case cmd : msg_CommonResultMessage => cmd.func(pr.get)
		case msg_PlusDateToResultMessage(data) => plusDateArgsToResult(data)(pr)
		case _ => (None, Some(ErrorCode.errorToJson("can not parse result")))
	}

	def plusDateArgsToResult(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		val d = (data \ "date").asOpt[Long].map (x =>x).getOrElse(new Date().getTime)
		(Some(pr.get + ("date" -> toJson(d))), None)
	}
}