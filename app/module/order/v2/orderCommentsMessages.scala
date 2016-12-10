package module.order.v2

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

abstract class msg_OrderCommentsCommand extends CommonMessage

object orderCommentsMessages {
	case class msg_PushOrderComment(data : JsValue) extends msg_OrderCommentsCommand
	case class msg_UpdateOrderComment(data : JsValue) extends msg_OrderCommentsCommand
	case class msg_PopOrderComment(data : JsValue) extends msg_OrderCommentsCommand
	case class msg_OverallOrderComment(data : JsValue) extends msg_OrderCommentsCommand
	case class msg_queryOrderComment(data : JsValue) extends msg_OrderCommentsCommand
	
	case class msg_OverallOrderLst(data : JsValue) extends msg_OrderCommentsCommand
	case class msg_OrdersOverallComments(data : JsValue) extends msg_OrderCommentsCommand
}