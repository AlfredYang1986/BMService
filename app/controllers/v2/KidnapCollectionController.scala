package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import module.test.testMessages._
import module.kidnap.v2.kidnapCollectionMessages._

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
			MessageRoutes(msg_UserCollectionLst(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
}