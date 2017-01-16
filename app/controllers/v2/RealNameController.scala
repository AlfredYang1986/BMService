package controllers.v2

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import module.profile.v2.ProfileMessages._
import pattern.ResultMessage.msg_CommonResultMessage
import module.auth.AuthMessage.msg_AuthQuery
import pattern.LogMessage.msg_log
import play.api.libs.json.Json.toJson
import module.realname.RealNameMessages._

object RealNameController extends Controller {
    def pushRealName = Action (request => requestArgsV2(request) { jv =>
        import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("push real name"))), jv)
            :: msg_pushRealName(jv) :: msg_CommonResultMessage() ::Nil, None)
    })
    def approveRealName = Action (request => requestArgsV2(request) { jv =>
        import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("push real name"))), jv)
            :: msg_approveRealName(jv) :: msg_CommonResultMessage() ::Nil, None)
    })
    def rejectRealName = Action (request => requestArgsV2(request) { jv =>
        import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("push real name"))), jv)
            :: msg_rejectRealName(jv) :: msg_CommonResultMessage() ::Nil, None)
    })
}
