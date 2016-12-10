package controllers.v2

import play.api._
import play.api.mvc._
import dongdamessages.MessageRoutes

import controllers.common.requestArgsQuery._

import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import pattern.ParallelMessage

import module.test.testMessages._

object SatterGatherTestController extends Controller {
	def testIndex = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
			import pattern.ParallelMessage.f
			MessageRoutes(
					ParallelMessage(
							MessageRoutes(msg_Test_1(jv) :: msg_Test_3(jv) :: Nil, None) :: 
							MessageRoutes(msg_Test_2(jv) :: Nil, None) :: Nil, f) 
					:: msg_Test_3(jv)
					:: msg_CommonResultMessage() :: Nil, None)
		})
}