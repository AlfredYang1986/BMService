package controllers.v3

import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import module.kidnap.v3.kidnapCollectionMessages._
import module.order.v3.orderCommentsMessages.msg_OverallOrderLst
import module.profile.v2.ProfileMessages.msg_OwnerLstNamePhoto
import module.timemanager.v3.TMMessages.msg_queryMultipleTMCommand
import pattern.LogMessage.msg_log
import pattern.ParallelMessage
import pattern.ResultMessage.msg_CommonResultMessage
import play.api.libs.json.Json.toJson
import play.api.mvc._

object KidnapCollectionController extends Controller {
	def collectService = Action (request => requestArgsV2(request) { jv => 
			import pattern.LogMessage.common_log
			import pattern.ResultMessage.lst_result
			MessageRoutes(msg_log(toJson(Map("method" -> toJson("collect service"))), jv)
				:: msg_CollectionService(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def uncollectService = Action (request => requestArgsV2(request) { jv => 
			import pattern.LogMessage.common_log
			import pattern.ResultMessage.lst_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("uncollect service"))), jv)
			    :: msg_UncollectionService(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def userCollectionLst = Action (request => requestArgsV2(request) { jv => 
			import module.kidnap.v3.kidnapModule.serviceResultMerge
			import pattern.LogMessage.common_log
			import pattern.ResultMessage.lst_result
			MessageRoutes(
                    msg_log(toJson(Map("method" -> toJson("user collect lst"))), jv) ::
					msg_UserCollectionLst(jv) :: msg_queryMultipleTMCommand(jv) ::
					ParallelMessage(
							MessageRoutes(msg_OverallOrderLst(jv) :: Nil, None) :: 
							MessageRoutes(msg_IsUserCollectLst(jv) :: Nil, None) :: 
							MessageRoutes(msg_OwnerLstNamePhoto(jv) :: Nil, None) :: Nil, serviceResultMerge) 
					:: msg_CommonResultMessage() :: Nil, None)
		})
}