package controllers.v2

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import module.profile.v2.ProfileMessages._
import pattern.ResultMessage.msg_CommonResultMessage
import module.auth.AuthMessage.msg_AuthQuery
import module.auth.AuthMessage.msg_AuthCheck
import pattern.LogMessage.msg_log
import play.api.libs.json.Json.toJson

object ProfileController extends Controller {
	def updateUserProfile = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
		    import pattern.LogMessage.common_log
		    MessageRoutes(msg_log(toJson(Map("method" -> toJson("update user profile"))), jv) :: msg_AuthCheck(jv)
			    :: msg_UpdateProfile(jv) :: msg_CommonResultMessage() ::Nil, None)
		})
	def queryUserProfile = Action (request => requestArgsV2(request) { jv => 
			import pattern.ResultMessage.common_result
            import pattern.LogMessage.common_log
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("query user profile"))), jv)
			    :: msg_QueryProfile(jv) :: msg_AuthQuery(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
}