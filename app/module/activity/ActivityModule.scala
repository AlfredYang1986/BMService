package module.activity

import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson
import com.mongodb.casbah.Imports._

import module.sercurity.Sercurity
import module.sms.smsModule

import util.dao.from
import util.dao._data_connection

import ActivityMessages._
import dongdamessages.MessageDefines
import pattern.ModuleTrait

import util.errorcode.ErrorCode

object ActivityModule extends ModuleTrait {
    def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
        case msg_QueryHistoryActivities(data) => queryHistoryActivities(data)
        case _ => ???
    }

    def queryHistoryActivities(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {



            null

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
}
