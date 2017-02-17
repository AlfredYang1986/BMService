package module.order.v2

import java.util.Date

import com.mongodb.casbah.Imports._

import dongdamessages.MessageDefines
import module.sercurity.Sercurity
import orderCommentsMessages._
import pattern.ModuleTrait
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson
import util.dao._data_connection
import util.dao.from
import util.errorcode.ErrorCode

object orderCommentsModule extends ModuleTrait {

	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case msg_PushOrderComment(data) => pushComments(data)
		case msg_UpdateOrderComment(data) => updateComments(data)
		case msg_PopOrderComment(data) => popComments(data)
		case msg_OverallOrderComment(data) => queryOverallComments(data)
		case msg_queryOrderComment(data) => queryComments(data)
		
		case msg_OverallOrderLst(data) => queryOverallOrderLst(data)(pr)
		case msg_OrdersOverallComments(data) => queryOneOverallOrder(data)(pr)
		case _ => ???
	}
	
    def pushComments(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val builder = MongoDBObject.newBuilder
            
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            builder += "order_id" -> order_id
            builder += "service_id" -> service_id
            builder += "owner_id" -> (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "user_id" -> (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "content" -> (data \ "content").asOpt[String].map (x => x).getOrElse("")
            builder += "points" -> ((data \ "points").asOpt[List[Int]].map (x => x).getOrElse(throw new Exception("wrong input"))).map (_.toFloat)
            
            builder += "comment_id" -> Sercurity.md5Hash(order_id + service_id + Sercurity.getTimeSpanWithMillSeconds)
            builder += "date" -> new Date().getTime
            
            _data_connection.getCollection("service_comments") += builder.result
            (Some(DB2Map(builder.result)), None)
        } catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
    
    def updateComments(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val comment_id = (data \ "comment_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            (from db() in "service_comments" where ("comment_id" -> comment_id) select (x => x)).toList match {
              case head :: Nil => {
//                  (data \ "accuracy").asOpt[Float].map (x => head += "accuracy" -> x.asInstanceOf[Number]).getOrElse(Unit)
//                  (data \ "communication").asOpt[Float].map (x => head += "communication" -> x.asInstanceOf[Number]).getOrElse(Unit)
//                  (data \ "professional").asOpt[Float].map (x => head += "professional" -> x.asInstanceOf[Number]).getOrElse(Unit)
//                  (data \ "hygiene").asOpt[Float].map (x => head += "hygiene" -> x.asInstanceOf[Number]).getOrElse(Unit)

                  (data \ "points").asOpt[List[Float]].map (x => head += "points" -> x).getOrElse(Unit)
                  (data \ "content").asOpt[String].map (x => head += "content" -> x).getOrElse(Unit)
                 
                  _data_connection.getCollection("service_comments").update(DBObject("comment_id" -> comment_id), head)
            	  (Some(DB2Map(head)), None)
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
    
    def popComments(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val comment_id = (data \ "comment_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "service_comments" where("comment_id" -> comment_id) select (x => x)).toList match {
              case head :: Nil => {
                  _data_connection.getCollection("service_comments") -= head
            	  (Some(DB2Map(head)), None)
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
  
    def average(lst : List[Double], count : Int) : List[Double] = lst map (x => x / count)
    	
    def overallAcc(r : List[Double], lst : List[List[Double]]) : List[Double] = lst match { 
    	case Nil => r
        case head :: tail => r match {
            case Nil => overallAcc(head, tail)
            case _ => overallAcc((r zip head).map (x => x._1 + x._2), tail) 
        }
    }
    
    def queryOverallComments(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
   
            val result = (from db() in "service_comments" where ("service_id" -> service_id) select (x => x.getAs[MongoDBList]("points").get.toList.asInstanceOf[List[Double]])).toList
			(Some(Map("service_id" -> toJson(service_id), "points" -> toJson(
                        average(overallAcc(Nil, result), result.length)))), None)
			
        } catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
    
    def queryComments(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
        try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
       
			(Some(Map("result" -> toJson(
                      (from db() in "service_comments" where ("service_id" -> service_id, "order_id" -> order_id) select (DB2Object(_))).toList))), None)
                      
        } catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    }
    
    def queryOverallOrderLst(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
    	try {
    		val lst = pr match {
    			case None => throw new Exception("wrong input")
    			case Some(m) => m.get("result").map (x => x.asOpt[List[JsValue]].get).getOrElse(throw new Exception("wrong input"))
    		}
    		
    		if (lst.isEmpty) {
    		
    			val fr : List[JsValue] = Nil
    			(Some(Map("result" -> toJson(fr))), None)	
    		} else {
	    		val condition = $or(lst map (x => DBObject("service_id" -> (x \ "service_id").asOpt[String].get)))
	    	
	    		val group = (from db() in "service_comments" where condition select { x => 
	    			val service_id = x.getAs[String]("service_id").get
	    			val points = x.getAs[MongoDBList]("points").get.toList.asInstanceOf[List[Double]]
	    			(service_id, points)
	    		}).toList.groupBy(_._1)
	    	
	    		val result = group map { iter => 
	    			val points = (iter._2.map (x => x._2))
	    			val avg = average(overallAcc(Nil, points), points.length)
	    			(iter._1, avg)
	    		}
	
	    		val fr = lst map { iter => 
	    			result.find(p => p._1 == (iter \ "service_id").asOpt[String].get).map { tmp => 
	    				toJson((iter.as[JsObject].value.toMap + ("points" -> toJson(tmp._2))))
	    			}.getOrElse(iter)
	    		}

	    		(Some(Map("result" -> toJson(fr))), None)
    		}

    	} catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
    	}
    }
    
    def queryOneOverallOrder(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
    	try {
    		val tmp_service_id = pr match {
    			case None => throw new Exception("wrong input")
    			case Some(m) => m.get("service_id").get.asOpt[String].get
    		}
    		
    		val group = (from db() in "service_comments" where ("service_id" -> tmp_service_id) select { x => 
    			val service_id = x.getAs[String]("service_id").get
    			val points = x.getAs[MongoDBList]("points").get.toList.asInstanceOf[List[Double]]
    			(service_id, points)
    		}).toList.groupBy(_._1)
    		
    		val result = (group map { iter => 
    			val points = (iter._2.map (x => x._2))
    			val avg = average(overallAcc(Nil, points), points.length)
    			(iter._1, avg)
    		}).head
    		
    		val fr = pr.get + ("points" -> toJson(result._2))
    		(Some(fr), None)
    		
    	} catch {
    		case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
    	}
    }
   
    def DB2Map(x : MongoDBObject) : Map[String, JsValue] = 
        Map("comment_id" -> toJson(x.getAs[String]("comment_id").get),
                   "order_id" -> toJson(x.getAs[String]("order_id").get),
                   "owner_id" -> toJson(x.getAs[String]("owner_id").get),
                   "user_id" -> toJson(x.getAs[String]("user_id").get),
                   "service_id" -> toJson(x.getAs[String]("service_id").get),
                   "content" -> toJson(x.getAs[String]("content").get),
                   "points" -> toJson(x.getAs[List[Float]]("points").get.toList),
                   "date" -> toJson(x.getAs[Number]("date").get.longValue))
    
    def DB2Object(x : MongoDBObject) : JsValue = 
        toJson(Map("comment_id" -> toJson(x.getAs[String]("comment_id").get),
                   "order_id" -> toJson(x.getAs[String]("order_id").get),
                   "owner_id" -> toJson(x.getAs[String]("owner_id").get),
                   "user_id" -> toJson(x.getAs[String]("user_id").get),
                   "service_id" -> toJson(x.getAs[String]("service_id").get),
                   "content" -> toJson(x.getAs[String]("content").get),
                   "points" -> toJson(x.getAs[List[Float]]("points").get.toList),
                   "date" -> toJson(x.getAs[Number]("date").get.longValue)))
}