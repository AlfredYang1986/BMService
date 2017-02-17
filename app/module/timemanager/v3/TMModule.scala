package module.timemanager.v3

import play.api.libs.json.{JsArray, JsObject, JsValue}
import play.api.libs.json.Json.toJson
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import module.sms.smsModule
import util.dao.from
import util.dao._data_connection
import dongdamessages.MessageDefines
import pattern.ModuleTrait
import util.errorcode.ErrorCode
import java.util.Date

import TMMessages._
import module.profile.v2.ProfileMessages._
import net.sf.ehcache.constructs.nonstop.NonstopExecutorServiceFactory

object TMPattern {
    case object daily extends TMPatternDefines(0, "daily")
    case object weekly extends TMPatternDefines(1, "weekly")
    case object monthly extends TMPatternDefines(2, "monthly")
    case object once extends TMPatternDefines(3, "once")
}

sealed abstract class TMPatternDefines(val t : Int, val des : String)

object TMModule extends ModuleTrait {

    def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
        case msg_pushTMCommand(data) => pushServiceTM(data)(pr)
        case msg_popTMCommand(data) => popServiceTM(data)(pr)
        case msg_queryTMCommand(data) => queryServiceTM(data)(pr)
        case msg_updateTMCommand(data) => updateServiceTM(data)(pr)
        case msg_queryMultipleTMCommand(data) => queryMultipleServiceTM(data)(pr)
        case _ => ???
    }

    def pushServiceTM(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val service_id = getServiceId(data, pr)

            val obj = Js2DBObject(data, service_id)
            _data_connection.getCollection("service_time") += obj

            pr match {
                case None => (Some(Map("result" -> toJson("success"))), None)
                case Some(re) => (Some(re), None)
            }

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def popServiceTM(data : JsValue)(pr : Option[Map[String, JsValue]])  : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val service_id = getServiceId(data, None)

            (from db() in "service_time" where ("service_id" -> service_id) select (x => x)).toList match {
                case head :: Nil => {
                    _data_connection.getCollection("service_time") -= head

                    pr match {
                        case None => (Some(Map("result" -> toJson("success"))), None)
                        case Some(re) => (Some(re), None)
                    }
                }
                case _ => throw new Exception("service not existing")
            }

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def queryServiceTM(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val service_id = getServiceId(data, None)

            (from db() in "service_time" where ("service_id" -> service_id) select (x => x)).toList match {
                case head :: Nil => {
                    val reVal = DB2JsValue(head)
                    println(reVal)
                    pr match {
                        case None => (Some(reVal + ("service_id" -> toJson(service_id))), None)
                        case Some(re) => {
                            println(re ++ reVal)
                            (Some(re ++ reVal), None)
                        }
                    }
                }
                case _ => throw new Exception("service not existing")
            }

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def queryMultipleServiceTM(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val service_lst = pr match {
                case None => throw new Exception("wrong input")
                case Some(m) => m.get("result").map (x => x.asOpt[List[JsValue]].get).getOrElse(throw new Exception("wrong input"))
            }

            if (service_lst.isEmpty) {
                (pr, None)
            } else {
                val conditions = $or(service_lst map (x => DBObject("service_id" ->(x \ "service_id").asOpt[String].get)))
                val tms = (from db() in "service_time" where conditions select(x => DB2JsValue(x) + ("service_id" -> toJson(x.getAs[String]("service_id").get)))).toList

                val reVal = service_lst map { x =>
                    import pattern.ParallelMessage.f
                    tms.find(p => p.get("service_id").get.asOpt[String].get == (x \ "service_id").asOpt[String].get) match {
                        case None => x
                        case Some(y) => toJson(f(x.as[JsObject].value.toMap :: y :: Nil))
                    }
                }

                (Some(Map("result" -> toJson(reVal))), None)
            }

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def updateServiceTM(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val service_id = getServiceId(data, None)

            (from db() in "service_time" where ("service_id" -> service_id) select (x => x)).toList match {
                case head :: Nil => {
                    val obj = Js2DBObject(data, service_id)
                    _data_connection.getCollection("service_time").update(head, obj)

                    pr match {
                        case None => (Some(Map("result" -> toJson("success"))), None)
                        case Some(re) => (Some(re), None)
                    }
                }
                case _ => throw new Exception("service not existing")
            }

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def getServiceId(data : JsValue, pr : Option[Map[String, JsValue]]) : String = (data \ "service_id").asOpt[String].map (x => x).getOrElse { pr match {
            case None => throw new Exception("wrong input")
            case Some(one) => one.get("service_id").map (x => x.asOpt[String].get).getOrElse(throw new Exception("wrong input"))
        }}

    def Js2DBObject(data : JsValue, service_id : String) : MongoDBObject = {
        val builder = MongoDBObject.newBuilder

        builder += "service_id" -> service_id

        val tm_builder = MongoDBList.newBuilder
        (data \ "tms").asOpt[List[JsValue]].map { arr => arr foreach { one =>
            val tmp = MongoDBObject.newBuilder
            tmp += "pattern" -> (one \ "pattern").asOpt[Int].map (x => x).getOrElse(throw new Exception("wrong input"))
            tmp += "startdate" -> (one \ "startdate").asOpt[Long].map (x => x).getOrElse(throw new Exception("wrong input"))
            tmp += "enddate" -> (one \ "enddate").asOpt[Long].map (x => x).getOrElse(-1)
            tmp += "starthours" -> (one \ "starthours").asOpt[Long].map (x => x).getOrElse(throw new Exception("wrong input"))
            tmp += "endhours" -> (one \ "endhours").asOpt[Long].map (x => x).getOrElse(throw new Exception("wrong input"))

            tm_builder += tmp.result

        }}.getOrElse("wrong input")
        builder += "tms" -> tm_builder.result

        builder.result
    }

    def DB2JsValue(obj : MongoDBObject) : Map[String, JsValue] = {
        val tms = obj.getAs[MongoDBList]("tms").get.toList.asInstanceOf[List[BasicDBObject]]

        Map("tms" ->
        toJson(tms.map { one =>
            toJson(Map("pattern" -> toJson(one.get("pattern").asInstanceOf[Number].intValue),
                        "startdate" -> toJson(one.get("startdate").asInstanceOf[Number].longValue),
                        "enddate" -> toJson(one.get("enddate").asInstanceOf[Number].longValue),
                        "starthours" -> toJson(one.get("starthours").asInstanceOf[Number].longValue),
                        "endhours" -> toJson(one.get("endhours").asInstanceOf[Number].longValue())))
        }))
    }
}
