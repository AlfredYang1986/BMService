package controllers.v2

import play.api._
import play.api.mvc._
import play.api.libs.json.Json.toJson
import play.api.libs.json.JsValue
import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import pattern.LogMessage.msg_log

object ActivityController {
    def historyActivities = Action (request => requestArgsV2(request) { jv =>
        import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("query historical activities"))), jv)
            :: msg_CommonResultMessage() :: Nil, None)
    })
}
