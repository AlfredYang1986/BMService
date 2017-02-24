package module.order.v2

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
import module.order.v2.orderMessages._
import module.kidnap.v2.kidnapModule
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
    case object reject extends orderStatusDefines(-9, "reject")
    case object unpaid extends orderStatusDefines(-1, "unpaid")
    case object ready extends orderStatusDefines(0, "ready")
    case object confirm extends orderStatusDefines(1, "confirm")
    case object paid extends orderStatusDefines(2, "paid")
    case object done extends orderStatusDefines(9, "done")
}

sealed abstract class orderStatusDefines(val t : Int, val des : String)

object orderModule extends ModuleTrait {
 
    val ddn = Akka.system(play.api.Play.current).actorOf(Props[DDNActor])

    def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case msg_PushOrder(data) => pushOrder(data)
        case msg_PushOrderAlipay(data) => pushOrderAlipay(data)
		case msg_payOrder(data) => payOrder(data)
		case msg_popOrder(data) => popOrder(data)
		case msg_updateOrder(data) => updateOrder(data)
		case msg_queryOrder(data) => queryOrders(data)
		case msg_acceptOrder(data) => acceptOrder(data)
		case msg_rejectOrder(data) => rejectOrder(data)
		case msg_accomplishOrder(data) => accomplishOrder(data)
		case _ => ???
	}

    def pushOrderAlipay(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)

            val order_id = Sercurity.md5Hash(user_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
            val obj = JsValue2DB(data, order_id)

            obj += "prepay_id" -> ""

            _data_connection.getCollection("orders") += obj

            DB2OptionJsValue(obj)
        } catch {
            case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }

	def pushOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception)
              
            val order_id = Sercurity.md5Hash(user_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
            val x = Future(JsValue2DB(data, order_id))
            val y = Future(WechatPayModule.prepayid(data, order_id))
            
            val obj = Await.result (x map (o => o), Timeout(1 second).duration).asInstanceOf[MongoDBObject]
            val js = Await.result (y map (v => v), Timeout(10 second).duration).asInstanceOf[JsValue]
           
            val prepay_id = (js \ "result" \ "prepay_id").asOpt[String].map (x => x).getOrElse("")
            
            obj += "prepay_id" -> prepay_id
            
            _data_connection.getCollection("orders") += obj
            
            DB2OptionJsValue(obj)
		} catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}

	def payOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
          
            (from db() in "orders" where ("order_id" -> order_id) select (x => x)).toList match {
              case head :: Nil => {
                  head += "status" -> orderStatus.paid.t.asInstanceOf[Number]
                 
                  { 
                      val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
                      val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
                      val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
                      val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")
                      
                      var content : Map[String, JsValue] = Map.empty
            		  content += "type" -> toJson(module.common.AcitionType.orderPushed.index)
            		  content += "sender_id" -> toJson(user_id)
            		  content += "date" -> toJson(new Date().getTime)
            	      content += "receiver_id" -> toJson(owner_id)
            		  content += "order_id" -> toJson(order_id)
            					content += "service_id" -> toJson(service_id)
                      content += "sign" -> toJson(Sercurity.md5Hash(user_id + order_id + service_id + Sercurity.getTimeSpanWithMillSeconds))
          					
              		  ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(owner_id).distinct),
                                               "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
                                               "from" -> toJson("dongda_master"))
                  }
                  
                  _data_connection.getCollection("orders").update(DBObject("order_id" -> order_id), head)
                  (Some(Map("result" -> toJson("success"))), None)
              }
              case _ => throw new Exception("not existing")
            }
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
                  (data \ "order_date").asOpt[Long].map (x => head += "order_date" -> x.longValue.asInstanceOf[Number]).getOrElse(Unit)
                  (data \ "is_read").asOpt[Int].map (x => head += "is_read" -> x.intValue.asInstanceOf[Number]).getOrElse(Unit)
                  
                  _data_connection.getCollection("orders").update(DBObject("order_id" -> order_id), head)
                  DB2OptionJsValue(head)
              }
              case _ => throw new Exception("wrong input")
            }
        } catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
	}
	
	def acceptOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")
            
            var content : Map[String, JsValue] = Map.empty
  			content += "type" -> toJson(module.common.AcitionType.orderAccecpted.index)
  			content += "sender_id" -> toJson(owner_id)
  			content += "date" -> toJson(new Date().getTime)
  			content += "receiver_id" -> toJson(user_id)
  			content += "order_id" -> toJson(order_id)
            content += "service_id" -> toJson(service_id)
            content += "sign" -> toJson(Sercurity.md5Hash(user_id + order_id + service_id + Sercurity.getTimeSpanWithMillSeconds))
					
    		ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(user_id).distinct),
                                     "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
                                     "from" -> toJson("dongda_master"))
            
            updateOrder(toJson(Map("order_id" -> toJson(order_id), "further_message" -> toJson(further_message), "status" -> toJson(orderStatus.confirm.t))))
        } catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
	}
	
	def rejectOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")
            
            var content : Map[String, JsValue] = Map.empty
  			content += "type" -> toJson(module.common.AcitionType.orderRejected.index)
  			content += "sender_id" -> toJson(owner_id)
  			content += "date" -> toJson(new Date().getTime)
  			content += "receiver_id" -> toJson(user_id)
  			content += "order_id" -> toJson(order_id)
            content += "service_id" -> toJson(service_id)
            content += "sign" -> toJson(Sercurity.md5Hash(user_id + order_id + service_id + Sercurity.getTimeSpanWithMillSeconds))
					
    		ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(user_id).distinct),
                                     "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
                                     "from" -> toJson("dongda_master"))
            
            updateOrder(toJson(Map("order_id" -> toJson(order_id), "further_message" -> toJson(further_message), "status" -> toJson(orderStatus.reject.t))))
        } catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
	}
	
	def accomplishOrder(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val further_message = (data \ "further_message").asOpt[String].map (x => x).getOrElse("")
            
            var content : Map[String, JsValue] = Map.empty
  			content += "type" -> toJson(module.common.AcitionType.orderAccomplished.index)
  			content += "sender_id" -> toJson(owner_id)
  			content += "date" -> toJson(new Date().getTime)
  			content += "receiver_id" -> toJson(user_id)
  			content += "order_id" -> toJson(order_id)
            content += "service_id" -> toJson(service_id)
            content += "sign" -> toJson(Sercurity.md5Hash(user_id + order_id + service_id + Sercurity.getTimeSpanWithMillSeconds))
					
    		ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(user_id).distinct),
                                     "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
                                     "from" -> toJson("dongda_master"))
            
            updateOrder(toJson(Map("order_id" -> toJson(order_id), "further_message" -> toJson(further_message), "status" -> toJson(orderStatus.done.t))))
        } catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
	}
	
	def queryOrders(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		      
        def serviceIdCondition(v : String) = "service_id" $eq v
        def userIdCondition(u : String) = "user_id" $eq u
        def statusCondition(s : Int) = "status" $eq s
        def dateCondition(d : Long) = "date" $gte d
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
            (con \ "user_id").asOpt[String].map (x => condition = conditionsAcc(condition, "user_id", x)).getOrElse(Unit)
            (con \ "owner_id").asOpt[String].map (x => condition = conditionsAcc(condition, "owner_id", x)).getOrElse(Unit)
            (con \ "date").asOpt[Long].map (x => condition = conditionsAcc(condition, "date", x)).getOrElse(Unit)
            (con \ "status").asOpt[Int].map (x => condition = conditionsAcc(condition, "status", x)).getOrElse(Unit)
            (con \ "order_date").asOpt[JsValue].map (x => condition = conditionsAcc(condition, "order_date", ((x \ "start").asOpt[Long].get, (x \ "end").asOpt[Long].get))).getOrElse(Unit)
       
            if (condition.isEmpty) throw new Exception("wrong input")
            else (Some(Map("result" -> toJson((from db() in "orders" where condition.get select (DB2JsValue(_))).toList))), None)
        } catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
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
        builder += "status" -> orderStatus.ready.t
      
        builder += "order_thumbs" -> (data \ "order_thumbs").asOpt[String].map (x => x).getOrElse(throw new Exception)
        builder += "order_title" -> (data \ "order_title").asOpt[String].map (x => x).getOrElse(throw new Exception)

        val order_date = MongoDBObject.newBuilder
        (data \ "order_date").asOpt[JsValue].map { x => 
            order_date += "start" -> (x \ "start").asOpt[Long].map (y => y).getOrElse(0.longValue)    
            order_date += "end" -> (x \ "end").asOpt[Long].map (y => y).getOrElse(0.longValue)    
        }.getOrElse(throw new Exception)
        builder += "order_date" -> order_date.result
        builder += "is_read" -> (data \ "is_read").asOpt[Int].map (x => x).getOrElse(0)
        builder += "order_id" -> order_id
        
        builder += "total_fee" -> (data \ "total_fee").asOpt[Float].map (x => x).getOrElse(throw new Exception)
        
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
	                       "order_date" -> toJson(Map("start" -> toJson(x.getAs[MongoDBObject]("order_date").get.getAs[Long]("start").get),
	                                                  "end" -> toJson(x.getAs[MongoDBObject]("order_date").get.getAs[Long]("end").get))),
	                       "is_read" -> toJson(x.getAs[Number]("is_read").get.intValue),
	                       "order_id" -> toJson(x.getAs[String]("order_id").get),
	                       "prepay_id" -> toJson(x.getAs[String]("prepay_id").map (x => x).getOrElse("")),
	                       "total_fee" -> toJson(x.getAs[Number]("total_fee").map (x => x.floatValue).getOrElse(0.01.asInstanceOf[Float])),
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
	                       "order_date" -> toJson(Map("start" -> toJson(x.getAs[MongoDBObject]("order_date").get.getAs[Long]("start").get),
	                                                  "end" -> toJson(x.getAs[MongoDBObject]("order_date").get.getAs[Long]("end").get))),
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
}