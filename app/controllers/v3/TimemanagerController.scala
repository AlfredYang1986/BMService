package controllers.v3


import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import pattern.ResultMessage.msg_CommonResultMessage
import pattern.ParallelMessage
import pattern.LogMessage.msg_log
import play.api.libs.json.Json.toJson

import module.timemanager.v3.TMMessages._

object TimemanagerController {
    def queryServiceTimeManagement = Action (request => requestArgsV2(request) { jv =>
        import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("query service time management"))), jv)
            :: msg_queryTMCommand(jv) :: msg_CommonResultMessage() :: Nil, None)
    })

    def pushServiceTimeManagement = Action (request => requestArgsV2(request) { jv =>
        import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("push service time management"))), jv)
            :: msg_pushTMCommand(jv) :: msg_CommonResultMessage() :: Nil, None)
    })

    def popServiceTimeManagement = Action (request => requestArgsV2(request) { jv =>
        import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("pop service time management"))), jv)
            :: msg_popTMCommand(jv) :: msg_CommonResultMessage() :: Nil, None)
    })

    def updateServiceTimeManagement = Action (request => requestArgsV2(request) { jv =>
        import pattern.ResultMessage.common_result
        import pattern.LogMessage.common_log
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("update service time managemnet"))), jv)
            :: msg_updateTMCommand(jv) :: msg_CommonResultMessage() :: Nil, None)
    })
}
