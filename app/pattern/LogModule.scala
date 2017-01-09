package pattern

import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson

import dongdamessages.MessageDefines
import LogMessage._

import util.errorcode.ErrorCode

object LogModule extends ModuleTrait {
    def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
        case cmd : msg_log => cmd.l(cmd.ls, cmd.data)
        case _ => (None, Some(ErrorCode.errorToJson("can not parse result")))
    }
}
