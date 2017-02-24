package module.order.v2

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

abstract class msg_OrderCommand extends CommonMessage

object orderMessages {
	case class msg_PushOrder(data : JsValue) extends msg_OrderCommand
	case class msg_PushOrderAlipay(data : JsValue) extends msg_OrderCommand
	case class msg_payOrder(data : JsValue) extends msg_OrderCommand
	case class msg_popOrder(data : JsValue) extends msg_OrderCommand
	case class msg_updateOrder(data : JsValue) extends msg_OrderCommand
	case class msg_queryOrder(data : JsValue) extends msg_OrderCommand
	case class msg_acceptOrder(data : JsValue) extends msg_OrderCommand
	case class msg_rejectOrder(data : JsValue) extends msg_OrderCommand
	case class msg_accomplishOrder(data : JsValue) extends msg_OrderCommand
}