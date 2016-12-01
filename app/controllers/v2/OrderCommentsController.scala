package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.order.v2.orderCommentsMessages._

object OrderCommentsController extends Controller {
	def pushOrderComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.common_result
		MessageRoutes(msg_PushOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
	def updateOrderComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.common_result
		MessageRoutes(msg_UpdateOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
	def popOrderComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.common_result
		MessageRoutes(msg_PopOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
	def queryComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.lst_result
		MessageRoutes(msg_queryOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
	def queryOverAllComments = Action (request => requestArgsV2(request) { jv => 
		import pattern.ResultMessage.common_result
		MessageRoutes(msg_OverallOrderComment(jv) :: msg_CommonResultMessage() :: Nil, None)
	})
}