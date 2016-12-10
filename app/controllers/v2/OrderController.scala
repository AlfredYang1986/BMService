package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.order.v2.orderMessages._
import module.kidnap.v2.kidnapMessages.msg_ServiceForOrders
import module.kidnap.v2.kidnapCollectionMessages.msg_IsUserCollectLst
import module.profile.v2.ProfileMessages.msg_OwnerLstNamePhoto
import module.profile.v2.ProfileMessages.msg_UserLstNamePhoto
import module.test.testMessages._
import pattern.ParallelMessage

import play.api.libs.json.Json.toJson

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

	def queryApplyOrders = queryOrders2
	def queryOwnerOrders = queryOrders2
		
	def queryOrders2 = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			import module.order.v2.orderModule.orderResultMerge
			import module.order.v2.orderModule.orderOrderMerge
			import module.order.v2.orderModule.orderFinalMerge
			
			val service_sub = ParallelMessage(
								MessageRoutes(msg_ServiceForOrders(jv) :: Nil, None) :: 
								MessageRoutes(msg_OwnerLstNamePhoto(jv) :: Nil, None) ::
								MessageRoutes(msg_IsUserCollectLst(jv) :: Nil, None) :: Nil, orderResultMerge)
								
			val order_sub = ParallelMessage(
								MessageRoutes(msg_UserLstNamePhoto(jv) :: Nil, None) :: 
								Nil, orderOrderMerge)
								
			val para = ParallelMessage(
							MessageRoutes(service_sub :: Nil, None) ::
							MessageRoutes(order_sub :: Nil, None) :: Nil, orderFinalMerge)
			
			MessageRoutes(msg_queryOrder(jv) :: para :: msg_CommonResultMessage() :: Nil, None)
		})
}