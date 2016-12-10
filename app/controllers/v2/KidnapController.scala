package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.kidnap.v2.kidnapMessages._
import module.profile.v2.ProfileMessages.{ msg_UpdateProfileWithoutResult, msg_ChangeToServiceProvider }
import pattern.ParallelMessage
import module.order.v2.orderCommentsMessages.msg_OverallOrderLst
import module.order.v2.orderCommentsMessages.msg_OrdersOverallComments
import module.kidnap.v2.kidnapCollectionMessages.msg_IsUserCollectLst
import module.kidnap.v2.kidnapCollectionMessages.msg_IsUserCollect
import module.profile.v2.ProfileMessages.msg_OwnerLstNamePhoto
import module.profile.v2.ProfileMessages.msg_OneOwnerNamePhoto
import module.test.testMessages._

object KidnapController extends Controller {
	def pushService = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_PushService(jv) :: msg_PublishService(jv) :: msg_UpdateProfileWithoutResult(jv) :: msg_ChangeToServiceProvider(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def popService = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_RevertService(jv) :: msg_PopService(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def updateService = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_RevertService(jv) :: msg_UpdateService(jv) :: msg_PublishService(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def searchServices  = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_SearchServices(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def queryMineServices = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_MineServices(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def queryMultiServices = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_QueryMultiServices(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def queryServiceDetail = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_QueryServiceDetail(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
		
	def searchServices2 = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			import module.kidnap.v2.kidnapModule.serviceResultMerge
			MessageRoutes(
					msg_SearchServices(jv) ::
					ParallelMessage(
							MessageRoutes(msg_OverallOrderLst(jv) :: Nil, None) :: 
							MessageRoutes(msg_IsUserCollectLst(jv) :: Nil, None) :: 
							MessageRoutes(msg_OwnerLstNamePhoto(jv) :: Nil, None) :: Nil, serviceResultMerge) 
					:: msg_CommonResultMessage() :: Nil, None)
		})
		
	def queryServiceDetail2 = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			import module.kidnap.v2.kidnapModule.detailResultMerge
			MessageRoutes(
					msg_QueryServiceDetail(jv) ::
					ParallelMessage(
							MessageRoutes(msg_OrdersOverallComments(jv) :: Nil, None) :: 
							MessageRoutes(msg_IsUserCollect(jv) :: Nil, None) :: 
							MessageRoutes(msg_OneOwnerNamePhoto(jv) :: Nil, None) :: Nil, detailResultMerge) 
					:: msg_CommonResultMessage() :: Nil, None)
		})
}