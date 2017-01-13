package controllers.v2

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.kidnap.v2.kidnapCollectionMessages._
import pattern.ParallelMessage
import module.profile.v2.ProfileMessages.msg_OwnerLstNamePhoto
import module.order.v2.orderCommentsMessages.msg_OverallOrderLst
import pattern.LogMessage.msg_log
import play.api.libs.json.Json.toJson

object KidnapCollectionController extends Controller {
	def collectService = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			import pattern.LogMessage.common_log
			MessageRoutes(msg_log(toJson(Map("method" -> toJson("collect service"))), jv)
				:: msg_CollectionService(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def uncollectService = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
            import pattern.LogMessage.common_log
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("uncollect service"))), jv)
			    :: msg_UncollectionService(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def userCollectionLst = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.lst_result
			import module.kidnap.v2.kidnapModule.serviceResultMerge
            import pattern.LogMessage.common_log
			MessageRoutes(
                    msg_log(toJson(Map("method" -> toJson("user collect lst"))), jv) ::
					msg_UserCollectionLst(jv) ::
					ParallelMessage(
							MessageRoutes(msg_OverallOrderLst(jv) :: Nil, None) :: 
							MessageRoutes(msg_IsUserCollectLst(jv) :: Nil, None) :: 
							MessageRoutes(msg_OwnerLstNamePhoto(jv) :: Nil, None) :: Nil, serviceResultMerge) 
					:: msg_CommonResultMessage() :: Nil, None)
		})
}