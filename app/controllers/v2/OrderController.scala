package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.order.v2.orderMessages._

object OrderController extends Controller {
	def pushOrder = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_PushOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def popOrder = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_popOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def queryOrders = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_queryOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def acceptOrder = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_acceptOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def rejectOrder = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_rejectOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})  
	def accomplishOrder = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_accomplishOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})  
	def queryApplyOrders = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_queryOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})  
	def queryOwnerOrders = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_queryOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})  
}