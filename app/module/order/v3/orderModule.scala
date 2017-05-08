package module.order.v3

import play.api.libs.concurrent.Akka
import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson

import java.util.Date
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import akka.util.Timeout
import akka.actor.Props

import module.webpay.WechatPayModule
import module.notification.DDNActor
import module.order.v3.orderMessages._
import module.kidnap.v3.kidnapModule
import module.sercurity.Sercurity

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode

import dongdamessages.MessageDefines
import pattern.ModuleTrait

import com.mongodb.casbah.Imports._
import module.notification.DDNNotifyUsers
import play.api.libs.json.JsObject

object orderStatus {
    case object expired extends orderStatusDefines(-1, "expired")

    case object ready extends orderStatusDefines(0, "ready")

    case object posted extends orderStatusDefines(1, "posted")
    case object rejected extends orderStatusDefines(2, "rejected")
    case object accepted extends orderStatusDefines(3, "accecpted")
    case object paid extends orderStatusDefines(4, "paid")
    case object cancelled extends orderStatusDefines(5, "cancelled")

    case object done extends orderStatusDefines(9, "done")
}

sealed abstract class orderStatusDefines(val t : Int, val des : String)

object orderModule extends ModuleTrait {
 
    val ddn = Akka.system(play.api.Play.current).actorOf(Props[DDNActor])

    def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
//		case msg_PushOrder(data) => pushOrder(data)
//        case msg_PushOrderAlipay(data) => pushOrderAlipay(data)
		case msg_payOrder(data) => payOrder(data)
		case msg_popOrder(data) => popOrder(data)
		case msg_updateOrder(data) => updateOrder(data)
		case msg_queryOrder(data) => queryOrders(data)
//		case msg_acceptOrder(data) => acceptOrder(data)
//		case msg_rejectOrder(data) => rejectOrder(data)
//		case msg_accomplishOrder(data) => accomplishOrder(data)
        case msg_splitOrderTimes(data) => splitOrderTimes(data)(pr)
        case msg_mineOrderForSplit(data) => mineOrderForSplit(data)

        case msg_PostOrder(data) => postOrder(data)
        case msg_CheckOwner(data) => v3_checkServiceOwner(data)
        case msg_CheckUser(data) => v3_checkServiceUser(data)
        case msg_Reject(data) => v3_reject(data)
    	case msg_Accept(data) => v3_accept(data)
        case msg_Cancel(data) => v3_Cancel(data)
        case msg_Prepay(data : JsValue) => v3_Prepay(data)
    	case msg_Postpay(data : JsValue) => payOrder(data)
		case _ => ???
	}

    /**
      * old for v2 order
      * @param data
      * @return
      */
//    def pushOrderAlipay(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
//        try {
//            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
//            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
//
//            val order_id = Sercurity.md5Hash(user_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
//            val obj = JsValue2DB(data, order_id)
//
//            obj += "prepay_id" -> ""
//
//            _data_connection.getCollection("orders") += obj
//
//            DB2OptionJsValue(obj)
//        } catch {
//            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
//        }
//    }

//	def pushOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
//		try {
//			val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
//            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
//
//            val order_id = Sercurity.md5Hash(user_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
//            val x = Future(JsValue2DB(data, order_id))
//            val y = Future(WechatPayModule.prepayid(data, order_id))
//
//            val obj = Await.result (x map (o => o), Timeout(1 second).duration).asInstanceOf[MongoDBObject]
//            val js = Await.result (y map (v => v), Timeout(10 second).duration).asInstanceOf[JsValue]
//
//            val prepay_id = (js \ "result" \ "prepay_id").asOpt[String].map (x => x).getOrElse("")
//
//            obj += "prepay_id" -> prepay_id
//
//            _data_connection.getCollection("orders") += obj
//
//            DB2OptionJsValue(obj)
//		} catch {
//  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
//		}
//	}

	def payOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            val result = updateOrder(toJson(Map("order_id" -> toJson(order_id), "status" -> toJson(orderStatus.paid.t))))

            sendStatusChangedNotification(result._1.get, module.common.AcitionType.order_v3_paid.index,
                                result._1.get.get("user_id").get.asOpt[String].get,
                                result._1.get.get("owner_id").get.asOpt[String].get,
                                order_id,
                                result._1.get.get("service_id").get.asOpt[String].get)

            result
		} catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
	
	def popOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "orders" where ("order_id" -> order_id) select (x => x)).toList match {
              case head :: Nil => {
                  _data_connection.getCollection("orders") -= head
                  (Some(Map("order_id" -> toJson(order_id))), None)
              }
              case _ => throw new Exception("wrong input")
            }
          
        } catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
	}

	def updateOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "orders" where ("order_id" -> order_id) select (x => x)).toList match {
              case head :: Nil => {
                  (data \ "status").asOpt[Int].map (x => head += "status" -> x.intValue.asInstanceOf[Number]).getOrElse(Unit)
                  (data \ "further_message").asOpt[String].map (x => head += "further_message" -> x).getOrElse(Unit)
                  (data \ "order_thumbs").asOpt[String].map (x => head += "order_thumbs" -> x).getOrElse(Unit)
//                  (data \ "order_date").asOpt[Long].map (x => head += "order_date" -> x.longValue.asInstanceOf[Number]).getOrElse(Unit)
                  (data \ "is_read").asOpt[Int].map (x => head += "is_read" -> x.intValue.asInstanceOf[Number]).getOrElse(Unit)
                  (data \ "order_date").asOpt[List[JsValue]].map (x => head += "order_date" -> JsOrderDate(data)).getOrElse(Unit)
                  (data \ "prepay_id").asOpt[String].map (x => head += "prepay_id" -> x).getOrElse(Unit)

                  _data_connection.getCollection("orders").update(DBObject("order_id" -> order_id), head)
                  DB2OptionJsValue(head)
              }
              case _ => throw new Exception("wrong input")
            }
        } catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
	}
	
//	def acceptOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
//		try {
//            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")
//
//            var content : Map[String, JsValue] = Map.empty
//  			content += "type" -> toJson(module.common.AcitionType.orderAccecpted.index)
//  			content += "sender_id" -> toJson(owner_id)
//  			content += "date" -> toJson(new Date().getTime)
//  			content += "receiver_id" -> toJson(user_id)
//  			content += "order_id" -> toJson(order_id)
//            content += "service_id" -> toJson(service_id)
//            content += "sign" -> toJson(Sercurity.md5Hash(user_id + order_id + service_id + Sercurity.getTimeSpanWithMillSeconds))
//
//    		ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(user_id).distinct),
//                                     "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
//                                     "from" -> toJson("dongda_master"))
//
//            updateOrder(toJson(Map("order_id" -> toJson(order_id), "further_message" -> toJson(further_message), "status" -> toJson(orderStatus.confirm.t))))
//        } catch {
//  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
//        }
//	}
//
//	def rejectOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
//		try {
//            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")
//
//            var content : Map[String, JsValue] = Map.empty
//  			content += "type" -> toJson(module.common.AcitionType.orderRejected.index)
//  			content += "sender_id" -> toJson(owner_id)
//  			content += "date" -> toJson(new Date().getTime)
//  			content += "receiver_id" -> toJson(user_id)
//  			content += "order_id" -> toJson(order_id)
//            content += "service_id" -> toJson(service_id)
//            content += "sign" -> toJson(Sercurity.md5Hash(user_id + order_id + service_id + Sercurity.getTimeSpanWithMillSeconds))
//
//    		ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(user_id).distinct),
//                                     "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
//                                     "from" -> toJson("dongda_master"))
//
//            updateOrder(toJson(Map("order_id" -> toJson(order_id), "further_message" -> toJson(further_message), "status" -> toJson(orderStatus.reject.t))))
//        } catch {
//  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
//        }
//	}
//
//	def accomplishOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
//		try {
//            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")
//
//            var content : Map[String, JsValue] = Map.empty
//  			content += "type" -> toJson(module.common.AcitionType.orderAccomplished.index)
//  			content += "sender_id" -> toJson(owner_id)
//  			content += "date" -> toJson(new Date().getTime)
//  			content += "receiver_id" -> toJson(user_id)
//  			content += "order_id" -> toJson(order_id)
//            content += "service_id" -> toJson(service_id)
//            content += "sign" -> toJson(Sercurity.md5Hash(user_id + order_id + service_id + Sercurity.getTimeSpanWithMillSeconds))
//
//    		ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(user_id).distinct),
//                                     "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
//                                     "from" -> toJson("dongda_master"))
//
//            updateOrder(toJson(Map("order_id" -> toJson(order_id), "further_message" -> toJson(further_message), "status" -> toJson(orderStatus.done.t))))
//        } catch {
//  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
//        }
//	}
	
	def queryOrders(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		      
        def serviceIdCondition(v : String) = "service_id" $eq v
        def userIdCondition(u : String) = "user_id" $eq u
        def statusCondition(s : Int) = "status" $eq s
        def dateCondition(d : Long) = "date" $lte d
        def ownIdCondition(o : String) = "owner_id" $eq o
        def orderDateCondition(o : (Long, Long)) = $and("order_date.start" $gte o._1, "order_date.end" $lt o._2)
     
        def conditionsAcc(o : Option[DBObject], key : String, value : Any) : Option[DBObject] = {
            val n = key match {
              case "service_id" => serviceIdCondition(value.asInstanceOf[String])
              case "user_id" => userIdCondition(value.asInstanceOf[String])
              case "owner_id" => ownIdCondition(value.asInstanceOf[String])
              case "status" => statusCondition(value.asInstanceOf[Int])
              case "date" => dateCondition(value.asInstanceOf[Long])
              case "order_date" => orderDateCondition(value.asInstanceOf[(Long, Long)])
              case _ => ???
            }
            
            if (o.isEmpty) Option(n)
            else Option($and(o.get, n))
        }
        
        try { 
        	val con = (data \ "condition")
            var condition : Option[DBObject] = Some("status" $ne 0)
            (con \ "service_id").asOpt[String].map (x => condition = conditionsAcc(condition, "service_id", x)).getOrElse(Unit)
            (con \ "order_user_id").asOpt[String].map (x => condition = conditionsAcc(condition, "user_id", x)).getOrElse(Unit)
            (con \ "owner_id").asOpt[String].map (x => condition = conditionsAcc(condition, "owner_id", x)).getOrElse(Unit)
            (con \ "status").asOpt[Int].map (x => condition = conditionsAcc(condition, "status", x)).getOrElse(Unit)
            (con \ "order_date").asOpt[JsValue].map (x => condition = conditionsAcc(condition, "order_date", ((x \ "start").asOpt[Long].get, (x \ "end").asOpt[Long].get))).getOrElse(Unit)

            val timespan = (con \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
            condition = conditionsAcc(condition, "date", timespan)

            val take = (data \ "take").asOpt[Int].getOrElse(5)
            val skip = (data \ "skip").asOpt[Int].getOrElse(0)

            val result =
                if (take > 0) (from db() in "orders" where condition.get).selectSkipTop (skip)(take)("data")(DB2JsValue(_)).toList
                else (from db() in "orders" where condition.get select (DB2JsValue(_))).toList

            if (condition.isEmpty) throw new Exception("wrong input")
            else (Some(Map("result" -> toJson(result), "date" -> toJson(timespan))), None)
        } catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
	}

    def JsOrderDate(data : JsValue) : MongoDBList = {
        val lst = (data \ "order_date").asOpt[List[JsValue]].map (x => x).getOrElse(throw new Exception("wrong input"))

        val dl = MongoDBList.newBuilder
        lst foreach { x =>
            val tmp = MongoDBObject.newBuilder
            tmp += "start" -> (x \ "start").asOpt[Long].map (y => y).getOrElse(throw new Exception("wrong input"))
            tmp += "end" -> (x \ "end").asOpt[Long].map (y => y).getOrElse(throw new Exception("wrong input"))

            dl += tmp.result
        }
        dl.result
//        val order_date = MongoDBObject.newBuilder
//        (data \ "order_date").asOpt[JsValue].map { x =>
//            order_date += "start" -> (x \ "start").asOpt[Long].map (y => y).getOrElse(0.longValue)
//            order_date += "end" -> (x \ "end").asOpt[Long].map (y => y).getOrElse(0.longValue)
//        }.getOrElse(throw new Exception)
//        order_date.result
    }

    def OrderDate2Js(x : MongoDBObject) : JsValue = {

        try {
            val lst = x.getAs[MongoDBList]("order_date").get.toList.asInstanceOf[List[BasicDBObject]]

            val result = lst map { x =>
                toJson(Map("start" -> toJson(x.getAs[Long]("start").get), "end" -> toJson(x.getAs[Long]("end").get)))
            }

            toJson(result)
        } catch {
            case ex : Exception =>
                toJson(
                    toJson(Map("start" -> toJson(x.getAs[MongoDBObject]("order_date").get.getAs[Long]("start").get),
                        "end" -> toJson(x.getAs[MongoDBObject]("order_date").get.getAs[Long]("end").get))) :: Nil)
        }
    }

	def JsValue2DB(data : JsValue, order_id : String) : MongoDBObject = {
        val builder = MongoDBObject.newBuilder
      
        val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
        val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
        
        builder += "user_id" -> user_id
        builder += "service_id" -> service_id

        val service = kidnapModule.queryKidnapServiceDetail(toJson(Map("service_id" -> toJson(service_id))))
        service._1 match {
        	case None => throw new Exception ("service not valid")
        	case Some(s) => {
        		s.get("owner_id").map { owner_id =>
        			builder += "owner_id" -> owner_id.asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
        		}.getOrElse(throw new Exception("wrong input"))
        	}
        }

        builder += "date" -> new Date().getTime
        builder += "status" -> orderStatus.posted.t
      
        builder += "order_thumbs" -> (data \ "order_thumbs").asOpt[String].map (x => x).getOrElse(throw new Exception)
        builder += "order_title" -> (data \ "order_title").asOpt[String].map (x => x).getOrElse(throw new Exception)

        builder += "order_date" -> JsOrderDate(data)
        builder += "is_read" -> (data \ "is_read").asOpt[Int].map (x => x).getOrElse(0)
        builder += "order_id" -> order_id
        
        builder += "total_fee" -> (data \ "total_fee").asOpt[Int].map (x => x).getOrElse(throw new Exception)
        
        builder += "further_message" -> (data \ "further_message").asOpt[String].map (x => x).getOrElse("")
        builder += "servant_no" -> (data \ "servant_no").asOpt[Int].map (x => x).getOrElse(1)

        builder.result
    }
   
    def DB2JsValue(x : MongoDBObject) : JsValue = {
//        val service = kidnapModule.queryKidnapServiceDetail(toJson(Map("service_id" -> toJson(x.getAs[String]("service_id").get))))
//        service._1 match {
//        	case None => throw new Exception("wrong input")
//        	case Some(s) => {
	            toJson(Map("user_id" -> toJson(x.getAs[String]("user_id").get),
	                       "service_id" -> toJson(x.getAs[String]("service_id").get),
	                       "owner_id" -> toJson(x.getAs[String]("owner_id").get),
	                       "date" -> toJson(x.getAs[Long]("date").get),
	                       "status" -> toJson(x.getAs[Number]("status").get.intValue),
	                       "order_thumbs" -> toJson(x.getAs[String]("order_thumbs").get),
	                       "order_date" -> OrderDate2Js(x),
	                       "is_read" -> toJson(x.getAs[Number]("is_read").get.intValue),
	                       "order_id" -> toJson(x.getAs[String]("order_id").get),
	                       "prepay_id" -> toJson(x.getAs[String]("prepay_id").map (x => x).getOrElse("")),
	                       "total_fee" -> toJson(x.getAs[Number]("total_fee").map (x => x.intValue).getOrElse(0)),
	                       "further_message" -> toJson(x.getAs[String]("further_message").map (x => x).getOrElse(""))
//	                       , "service" -> toJson(s)
	                  ))
//          	}
//        }
    }

    def DB2OptionJsValue(x : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = {
//        val service = kidnapModule.queryKidnapServiceDetail(toJson(Map("service_id" -> toJson(x.getAs[String]("service_id").get))))
//        service._1 match {
//        	case None => service
//        	case Some(s) => {
        		val re = Map("user_id" -> toJson(x.getAs[String]("user_id").get),
	                       "service_id" -> toJson(x.getAs[String]("service_id").get),
	                       "owner_id" -> toJson(x.getAs[String]("owner_id").get),
	                       "date" -> toJson(x.getAs[Long]("date").get),
	                       "status" -> toJson(x.getAs[Number]("status").get.intValue),
	                       "order_thumbs" -> toJson(x.getAs[String]("order_thumbs").get),
                           "order_date" -> OrderDate2Js(x),
	                       "is_read" -> toJson(x.getAs[Number]("is_read").get.intValue),
	                       "order_id" -> toJson(x.getAs[String]("order_id").get),
	                       "prepay_id" -> toJson(x.getAs[String]("prepay_id").map (x => x).getOrElse("")),
	                       "servant_no" -> toJson(x.getAs[Number]("servant_no").map (x => x.intValue).getOrElse(1)),
	                       "total_fee" -> toJson(x.getAs[Number]("total_fee").map (x => x.floatValue).getOrElse(0.01.asInstanceOf[Float])),
	                       "further_message" -> toJson(x.getAs[String]("further_message").map (x => x).getOrElse(""))
//	                       , "service" -> toJson(s)
	                       )
        		(Some(re), None)
//        	}
//        }
    }

    /**
      * merge data functions
      * @param rst
      * @return
      */
    def orderResultMerge(rst : List[Map[String, JsValue]]) : Map[String, JsValue] = {
  
    	println(s"order result merge: $rst")
    	try {
    		val s = rst.find(x => x.get("message").get.asOpt[String].get == "service_for_order").get
	    	val c = rst.find(x => x.get("message").get.asOpt[String].get == "collections_lst").get
	    	val n = rst.find(x => x.get("message").get.asOpt[String].get == "profile_name_photo").get
	   
	    	val order = s.get("result").get.asOpt[List[JsValue]].get.map (x => (x \ "order").asOpt[JsValue].get)
	    	val service = s.get("result").get.asOpt[List[JsValue]].get.map (x => (x \ "service").asOpt[JsValue].get)
	    	val coll_lst = c.get("result").get.asOpt[List[JsValue]].get
	    	val service_owner_name_photo = n.get("result").get.asOpt[List[JsValue]].get
	    	
			import pattern.ParallelMessage.f
			val s_lst = (service zip coll_lst zip service_owner_name_photo)
						.map (tmp => f(tmp._1._1.as[JsObject].value.toMap :: 
								tmp._1._2.as[JsObject].value.toMap :: 
								tmp._2.as[JsObject].value.toMap :: Nil))
			
			Map("message" -> toJson("order_service"), "result" -> toJson(Map("order" -> toJson(order), "service" -> toJson(s_lst))))
    	} catch {
    		case ex : Exception => Map("message" -> toJson("order_service"), "result" -> toJson(List[JsValue]()))
    	}
    }
    
    def orderOrderMerge(rst : List[Map[String, JsValue]]) : Map[String, JsValue] = {
		import pattern.ParallelMessage.f
		f(rst) + ("message" -> toJson("profile_name_photo"))   	
    }
    
    def orderFinalMerge(rst : List[Map[String, JsValue]]) : Map[String, JsValue] = {
		import pattern.ParallelMessage.f
	
		try {
			val os = rst.find(x => x.get("message").get.asOpt[String].get == "order_service").get
			val np = rst.find(x => x.get("message").get.asOpt[String].get == "profile_name_photo").get
	
	    	val order = os.get("result").get.asOpt[JsValue].map (x => (x \ "order").asOpt[List[JsValue]].get).getOrElse(throw new Exception("wrong input"))
	    	val service = os.get("result").get.asOpt[JsValue].map (x => (x \ "service").asOpt[List[JsValue]].get).getOrElse(throw new Exception("wrong input"))
	    	val np_lst = np.get("result").get.asOpt[List[JsValue]].get
		
	    	val order_lst = (order zip service) map (tmp => tmp._1.as[JsObject].value.toMap + ("service" -> tmp._2))
	    	val result = (order_lst zip np_lst) map (tmp => f(tmp._1 :: tmp._2.as[JsObject].value.toMap :: Nil))
	    	Map("result" -> toJson(result))
		} catch {
    		case ex : Exception => Map("message" -> toJson("order_service"), "result" -> toJson(List[JsValue]()))
    	}
    }

    def mineOrderForSplit(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(5)
            val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)

            val a = data.as[JsObject].value.toMap + ("take" -> take) + ("skip" -> skip)
            queryOrders(data)

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def splitOrderTimes(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            println(s"order result split with time")

            pr match {
                case Some(m) => {

                    val lst : List[JsValue] = m.get("result").get.asOpt[List[JsValue]].get
                    val result = lst.map {x : JsValue =>
                        val od = (x \ "order_date").asOpt[List[JsValue]].get
                        od map (y => Map(
                            "service_id" -> toJson(x \ "service_id"),
                            "order_id" -> toJson(x \ "order_id"),
                            "user" -> toJson(Map("screen_name" -> toJson(x \ "screen_name"),
                                                 "screen_photo" -> toJson(x \ "screen_photo"))),
                            "owner" -> toJson(Map("screen_name" -> toJson(x \ "service" \ "screen_name"),
                                                  "screen_photo" -> toJson(x \ "service" \ "screen_photo"))),
//                            "screen_name" -> toJson(x \ "service" \ "screen_name"),
//                            "screen_photo" -> toJson(x \ "service" \ "screen_photo"),
//                            "cans" -> toJson(x \ "service" \ "cans"),
                            "order_title" -> toJson(x \ "order_title"),
                            "order_thumbs" -> toJson(x \ "order_thumbs"),
                            "start" -> toJson(y \ "start"),
                            "end" -> toJson(y \ "end")
                        ))
                    }.flatten


                    (Some(Map("result" -> toJson(result))), None)
                }
                case None => throw new Exception("unknow error")
            }

        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    /**
      * for v3 new version orders
      */
    def sendStatusChangedNotification(m : Map[String, JsValue],
                                      t : Int,
                                      sender_id : String,
                                      receiver_id : String,
                                      order_id : String,
                                      service_id : String) = {

        var content : Map[String, JsValue] = Map.empty
        content += "type" -> toJson(module.common.AcitionType.orderPushed.index)
        content += "sender_id" -> toJson(sender_id)
        content += "date" -> toJson(new Date().getTime)
        content += "receiver_id" -> toJson(receiver_id)
        content += "order_id" -> toJson(order_id)
        content += "service_id" -> toJson(service_id)
        content += "content" -> toJson(m)
        content += "sign" -> toJson(Sercurity.md5Hash(sender_id + order_id + service_id + Sercurity.getTimeSpanWithMillSeconds))

        ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(receiver_id).distinct),
            "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
            "from" -> toJson("dongda_master"))
    }

    def postOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)

            val order_id = Sercurity.md5Hash(user_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
            val obj = JsValue2DB(data, order_id)
            obj += "prepay_id" -> ""

            _data_connection.getCollection("orders") += obj

            val result = DB2OptionJsValue(obj)

            sendStatusChangedNotification(result._1.get, module.common.AcitionType.order_v3_posted.index,
                                            user_id, result._1.get.get("owner_id").get.asOpt[String].get,
                                            order_id, service_id)

            result
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def v3_checkServiceOwner(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "orders" where ("order_id" -> order_id) select (x => x.getAs[String]("owner_id").get)).toList match {
                case head :: _ =>
                    if (head == user_id) (Some(Map("status" -> toJson("ok"))), None)
                    else throw new Exception("unknown error")
                case _ => throw new Exception("unknown error")
            }
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def v3_checkServiceUser(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            (from db() in "orders" where ("order_id" -> order_id) select (x => x.getAs[String]("user_id").get)).toList match {
                case head :: _ =>   if (user_id == head) (Some(Map("status" -> toJson("ok"))), None)
                                    else throw new Exception("unknown error")
                case _ => throw new Exception("unknown error")
            }
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def v3_reject(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")

            val args = toJson(Map("order_id" -> toJson(order_id),
                                  "status" -> toJson(orderStatus.rejected.t),
                                  "further_message" -> toJson(further_message)))
            val result = updateOrder(args)

            sendStatusChangedNotification(result._1.get, module.common.AcitionType.order_v3_rejected.index,
                                            result._1.get.get("owner_id").get.asOpt[String].get,
                                            result._1.get.get("user_id").get.asOpt[String].get,
                                            order_id,
                                            result._1.get.get("service_id").get.asOpt[String].get)

            result
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def v3_accept(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")

            val args = toJson(Map("order_id" -> toJson(order_id),
                                  "status" -> toJson(orderStatus.accepted.t),
                                  "further_message" -> toJson(further_message)))
            val result = updateOrder(args)

            sendStatusChangedNotification(result._1.get, module.common.AcitionType.order_v3_accepted.index,
                                            result._1.get.get("owner_id").get.asOpt[String].get,
                                            result._1.get.get("user_id").get.asOpt[String].get,
                                            order_id,
                                            result._1.get.get("service_id").get.asOpt[String].get)

            result
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def v3_Cancel(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")

            val args = toJson(Map("order_id" -> toJson(order_id),
                                  "status" -> toJson(orderStatus.cancelled.t),
                                  "further_message" -> toJson(further_message)))
            val result = updateOrder(args)

            sendStatusChangedNotification(result._1.get, module.common.AcitionType.order_v3_cancel.index,
                                            result._1.get.get("user_id").get.asOpt[String].get,
                                            result._1.get.get("owner_id").get.asOpt[String].get,
                                            order_id,
                                            result._1.get.get("service_id").get.asOpt[String].get)

            result
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

    def v3_Prepay(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val pay_method = (data \ "pay_method").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

            val result = pay_method match {
                case "wechat" => {
                    val js = WechatPayModule.prepayid(data, order_id)
                    println(s"get wechat pay prepayid : $js")
                    val prepay_id = (js \ "result" \ "prepay_id").asOpt[String].map (x => x).getOrElse(throw new Exception("prepay id error"))
                    updateOrder(toJson(Map("order_id" -> toJson(order_id), "prepay_id" -> toJson(prepay_id))))
                }
                case "alipay" => updateOrder(toJson(Map("order_id" -> toJson(order_id), "prepay_id" -> toJson(""))))
            }

//            sendStatusChangedNotification(result._1.get, module.common.AcitionType.order_v3_cancel.index,
//                                result._1.get.get("user_id").get.asOpt[String].get,
//                                result._1.get.get("owner_id").get.asOpt[String].get,
//                                order_id,
//                                result._1.get.get("service_id").get.asOpt[String].get)

            result
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
}