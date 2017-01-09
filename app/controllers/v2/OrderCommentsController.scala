package controllers.v2

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.order.v2.orderCommentsMessages._
import pattern.LogMessage.msg_log
import play.api.libs.json.Json.toJson

object OrderCommentsController extends Controller {
	def pushOrderComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.common_result
		import pattern.LogMessage.common_log
		MessageRoutes(msg_log(toJson(Map("method" -> toJson("push order comment"))), jv)
		    :: msg_PushOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
	def updateOrderComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
		MessageRoutes(msg_log(toJson(Map("method" -> toJson("update order comment"))), jv)
            :: msg_UpdateOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
	def popOrderComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
		MessageRoutes(msg_log(toJson(Map("method" -> toJson("pop order comments"))), jv)
            :: msg_PopOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
	def queryComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.lst_result
        import pattern.LogMessage.common_log
		MessageRoutes(msg_log(toJson(Map("method" -> toJson("query comments"))), jv)
            :: msg_queryOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
	def queryOverAllComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
		MessageRoutes(msg_log(toJson(Map("method" -> toJson("query over all comments"))), jv)
            :: msg_OverallOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
}