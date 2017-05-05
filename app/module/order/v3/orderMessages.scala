package module.order.v3

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

abstract class msg_OrderCommand extends CommonMessage

object orderMessages {
	// only for version 2
	case class msg_PushOrder(data : JsValue) extends msg_OrderCommand
	case class msg_PushOrderAlipay(data : JsValue) extends msg_OrderCommand
	case class msg_payOrder(data : JsValue) extends msg_OrderCommand
	case class msg_popOrder(data : JsValue) extends msg_OrderCommand
	case class msg_updateOrder(data : JsValue) extends msg_OrderCommand
	case class msg_queryOrder(data : JsValue) extends msg_OrderCommand
	case class msg_acceptOrder(data : JsValue) extends msg_OrderCommand
	case class msg_rejectOrder(data : JsValue) extends msg_OrderCommand
	case class msg_accomplishOrder(data : JsValue) extends msg_OrderCommand
	case class msg_splitOrderTimes(data : JsValue) extends msg_OrderCommand
    case class msg_mineOrderForSplit(data : JsValue) extends msg_OrderCommand

	// for version 3 order server
	case class msg_PostOrder(data : JsValue) extends msg_OrderCommand		// new
//	case class msg_RefreshOrder(data : JsValue) extends msg_OrderCommand	// update
	case class msg_CheckOwner(data : JsValue) extends msg_OrderCommand
	case class msg_CheckUser(data : JsValue) extends msg_OrderCommand
	case class msg_Reject(data : JsValue) extends msg_OrderCommand
	case class msg_Accept(data : JsValue) extends msg_OrderCommand
	case class msg_Cancel(data : JsValue) extends msg_OrderCommand
	case class msg_Prepay(data : JsValue) extends msg_OrderCommand
	case class msg_Postpay(data : JsValue) extends msg_OrderCommand
}