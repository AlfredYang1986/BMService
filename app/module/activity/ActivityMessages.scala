package module.activity

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

abstract class msg_ActivityCommand extends CommonMessage

object ActivityMessages {
    case class msg_QueryHistoryActivities(data : JsValue) extends msg_ActivityCommand
}