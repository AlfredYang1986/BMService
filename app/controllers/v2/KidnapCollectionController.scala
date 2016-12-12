package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.test.testMessages._
import module.kidnap.v2.kidnapCollectionMessages._
import pattern.ParallelMessage
import module.profile.v2.ProfileMessages.msg_OwnerLstNamePhoto
import module.order.v2.orderCommentsMessages.msg_OverallOrderLst

object KidnapCollectionController extends Controller {
	def collectService = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_CollectionService(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def uncollectService = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_UncollectionService(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def userCollectionLst = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			import module.kidnap.v2.kidnapModule.serviceResultMerge
			MessageRoutes(
					msg_UserCollectionLst(jv) ::
					ParallelMessage(
							MessageRoutes(msg_OverallOrderLst(jv) :: Nil, None) :: 
							MessageRoutes(msg_IsUserCollectLst(jv) :: Nil, None) :: 
							MessageRoutes(msg_OwnerLstNamePhoto(jv) :: Nil, None) :: Nil, serviceResultMerge) 
					:: msg_CommonResultMessage() :: Nil, None)
		})
}