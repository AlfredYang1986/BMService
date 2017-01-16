package module.realname

import play.api.libs.json.Json
import play.api.libs.json.Json.toJson
import play.api.libs.json.JsValue
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

import RealNameMessages._
import pattern.ModuleTrait
import dongdamessages.MessageDefines

object realNameStatus {
    case object pushed extends RealNameAuthDefines(0, "status base")
    case object approved extends RealNameAuthDefines(1, "approved")
    case object rejected extends RealNameAuthDefines(2, "rejected")
}

sealed abstract class RealNameAuthDefines(val t : Int, val des : String)

object RealNameModule extends ModuleTrait {
    def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
        case msg_pushRealName(data) => pushRealName(data)
        case msg_approveRealName(data) => approveRealName(data)
        case msg_rejectRealName(data) => rejectRealName(data)
        case _ => ???
    }

    def pushRealName(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val real_name = (data \ "real_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val social_id = (data \ "social_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
                case head :: Nil => {
                    val builder = MongoDBObject.newBuilder
                    builder += "real_name" -> real_name
                    builder += "social_id" -> social_id
                    //                  builder += "status" -> realNameStatus.pushed.t
                    builder += "status" -> realNameStatus.approved.t

                    head += "real_name" -> builder.result

                    _data_connection.getCollection("users").update(DBObject("user_id" -> user_id), head)
//                    toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
                    (Some(Map("result" -> toJson("success"))), None)
                }
                case _ => throw new Exception("not existing")
            }
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def approveRealName(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue])  = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
                case head :: Nil => {
                    val x = head.getAs[MongoDBObject]("real_name").map (x => x).getOrElse(throw new Exception("not existing"))
                    x += "status" -> realNameStatus.approved.t.asInstanceOf[Number]
                    head += "real_name" -> x

                    _data_connection.getCollection("users").update(DBObject("user_id" -> user_id), head)
//                    toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
                    (Some(Map("result" -> toJson("success"))), None)
                }
                case _ => throw new Exception("not existing")
            }
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def rejectRealName(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
                case head :: Nil => {
                    val x = head.getAs[MongoDBObject]("real_name").map (x => x).getOrElse(throw new Exception("not existing"))
                    x += "status" -> realNameStatus.rejected.t.asInstanceOf[Number]
                    head += "real_name" -> x

                    _data_connection.getCollection("users").update(DBObject("user_id" -> user_id), head)
//                    toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
                    (Some(Map("result" -> toJson("success"))), None)
                }
                case _ => throw new Exception("not existing")
            }
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
}