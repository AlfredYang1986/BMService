package module.realname

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

import util.errorcode.ErrorCode

abstract class msg_RealNameCommand extends CommonMessage

object RealNameMessages {
    case class msg_pushRealName(data : JsValue) extends msg_RealNameCommand
    case class msg_approveRealName(data : JsValue) extends msg_RealNameCommand
    case class msg_rejectRealName(data : JsValue) extends msg_RealNameCommand
}