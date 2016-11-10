package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.kidnap.v2.kidnapMessages._
import module.profile.v2.ProfileMessages.msg_UpdateProfileWithoutResult

object KidnapController extends Controller {
	def pushService = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			MessageRoutes(msg_PushService(jv) :: msg_PublishService(jv) :: msg_UpdateProfileWithoutResult(jv) :: msg_CommonResultMessage() :: Nil, None)
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
}