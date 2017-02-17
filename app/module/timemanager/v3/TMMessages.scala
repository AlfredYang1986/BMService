package module.timemanager.v3

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

import util.errorcode.ErrorCode

abstract class msg_TMCommand extends CommonMessage

object TMMessages {
    case class msg_pushTMCommand(data : JsValue) extends msg_TMCommand
    case class msg_updateTMCommand(data : JsValue) extends msg_TMCommand
    case class msg_popTMCommand(data : JsValue) extends msg_TMCommand
    case class msg_queryTMCommand(data : JsValue) extends msg_TMCommand
    case class msg_queryMultipleTMCommand(data : JsValue) extends msg_TMCommand
}