package module.kidnap.v2

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

import util.errorcode.ErrorCode

abstract class msg_KidnapServiceCommand extends CommonMessage

object kidnapMessages {
	case class msg_PushService(data : JsValue) extends msg_KidnapServiceCommand
	case class msg_PopService(data : JsValue) extends msg_KidnapServiceCommand
	case class msg_RevertService(data : JsValue) extends msg_KidnapServiceCommand
	case class msg_UpdateService(data : JsValue) extends msg_KidnapServiceCommand
	case class msg_PublishService(data : JsValue) extends msg_KidnapServiceCommand
	case class msg_QueryServiceDetail(data : JsValue) extends msg_KidnapServiceCommand
	case class msg_QueryMultiServices(data : JsValue) extends msg_KidnapServiceCommand
	case class msg_SearchServices(data : JsValue) extends msg_KidnapServiceCommand
	case class msg_MineServices(data : JsValue) extends msg_KidnapServiceCommand
	
	case class msg_ServiceForOrders(data : JsValue) extends msg_KidnapServiceCommand
}