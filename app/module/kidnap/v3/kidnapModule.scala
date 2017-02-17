package module.kidnap.v3

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

import play.api.Play.current
import play.api.libs.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.util.Random

import java.util.Date

import module.sercurity.Sercurity

import dongdamessages.MessageDefines
import pattern.ModuleTrait
import kidnapMessages._

object weekDay {  
    case object Mon extends weekDayDefines(0, "Mongday")
    case object Tus extends weekDayDefines(1, "Tuesday")
    case object Wed extends weekDayDefines(2, "Wednesday")
    case object Thu extends weekDayDefines(3, "Thursday")
    case object Fri extends weekDayDefines(4, "Friday")
    case object Sat extends weekDayDefines(5, "Saturday")
    case object Sun extends weekDayDefines(6, "Sunday")
}

sealed abstract class weekDayDefines(val t : Int, val des : String)

object kidnapModule extends ModuleTrait {
 
	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case msg_PushService(data) => pushKidnapService(data)
		case msg_RevertService(data) => revertKidnapService(data)
		case msg_UpdateService(data) => updateKidnapService(data)
		case msg_PublishService(data) => publishKidnapService(data)(pr)
		case msg_PopService(data) => popKidnapService(data)
		case msg_QueryServiceDetail(data) => queryKidnapServiceDetail(data)
		case msg_QueryMultiServices(data) => queryMultipleService(data)
		case msg_SearchServices(data) => searchKidnapService(data)
		case msg_MineServices(data) => mineKidnapService(data)
		
		case msg_ServiceForOrders(data) => paralleServiceForOrder(data)(pr)
		case _ => ???
	}
	
    def queryServiceStatus(service_id : Option[String]) : (Int, MongoDBObject) = {
        service_id match {
          case Some(x) => (from db() in "kidnap" where ("service_id" -> x) select (tmp => tmp)).toList match {
                            case Nil => (kidnapServiceStatus.none.t, null)
                            case head :: Nil => (head.getAs[Number]("status").get.intValue, head)
                            case _ => null
                          }
          case None => (kidnapServiceStatus.none.t, null)
        }
    }

//  	def pushKidnapService(data : JsValue) : JsValue = {
  	def pushKidnapService(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).push(data, origin)
  	}
  	
//  	def pushKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : JsValue = {
  	def pushKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    try {
      	    val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("unknown user"))
  	        val service_id = Sercurity.md5Hash(owner_id + Sercurity.getTimeSpanWithMillSeconds)
  	        
  	        val service_builder = MongoDBObject.newBuilder
  	        service_builder += "service_id" -> service_id
  	        service_builder += "owner_id" -> owner_id
  	        
  	        // val offer_date = MongoDBObject.newBuilder
  	        // (data \ "offer_date").asOpt[JsValue].map { date => 
  	        //     offer_date += "start" -> (date \ "start").asOpt[Long].map (tmp => tmp).getOrElse(0.longValue)   
  	        //     offer_date += "end" -> (date \ "end").asOpt[Long].map (tmp => tmp).getOrElse(0.longValue)   
  	        // }.getOrElse {
  	        //     offer_date += "start" -> 0.longValue
  	        //     offer_date += "end" -> 0.longValue
  	        // }

//            val offer_date = MongoDBList.newBuilder
//            (data \ "offer_date").asOpt[List[JsValue]].map { lst =>
//                lst map { date =>
//                    val one_date = MongoDBObject.newBuilder
//                    one_date += "day" -> (date \ "day").asOpt[Int].get
//
//                    val se_lst = MongoDBList.newBuilder
//                    (date \ "occurance").asOpt[List[JsValue]].get map { occ =>
//                        val se = MongoDBObject.newBuilder
//                        se += "start" -> (occ \ "start").asOpt[Int].get
//                        se += "end" -> (occ \ "end").asOpt[Int].get
//                        se_lst += se.result
//                    }
//                    one_date += "occurance" -> se_lst.result
//			        offer_date += one_date.result
//                }
//            }.getOrElse {
//                import weekDay._
//                (Mon :: Tus :: Wed :: Thu :: Fri :: Sat :: Sun :: Nil) map { day =>
//                    val one_date = MongoDBObject.newBuilder
//                    one_date += "day" -> day.t.asInstanceOf[Number]
//
//                    val se_lst = MongoDBList.newBuilder
//
//                    val se = MongoDBObject.newBuilder
//                    se += "start" -> 0
//                    se += "end" -> 2400
//                    se_lst += se.result
//
//                    one_date += "occurance" -> se_lst.result
//
//	                offer_date += one_date.result
//                }
//            }
//  	        service_builder += "offer_date" -> offer_date.result
  	       
  	        val location = MongoDBObject.newBuilder
  	        (data \ "location").asOpt[JsValue].map { loc => 
  	            location += "latitude" -> (loc \ "latitude").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	            location += "longtitude" -> (loc \ "longtitude").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	        }.getOrElse {
  	            location += "latitude" -> 0.floatValue
  	            location += "longtitude" -> 0.floatValue
  	        }
  	        service_builder += "location" -> location.result
  	        
  	        service_builder += "comments" -> MongoDBList.newBuilder.result
  	        (data \ "title").asOpt[String].map (tmp => service_builder += "title" -> tmp).getOrElse(throw new Exception("wrong input"))
  	        (data \ "description").asOpt[String].map (tmp => service_builder += "description" -> tmp).getOrElse(service_builder += "description" -> "")
  	        (data \ "capacity").asOpt[Int].map (tmp => service_builder += "capacity" -> tmp).getOrElse(service_builder += "capacity" -> 0.intValue)
  	        (data \ "price").asOpt[Float].map (tmp => service_builder += "price" -> tmp).getOrElse(service_builder += "price" -> 0.floatValue)

  	        service_builder += "status" -> kidnapServiceStatus.offine.t
  	        service_builder += "rate" -> 0.floatValue
  	     
  	        (data \ "cans_cat").asOpt[Long].map (x => service_builder += "cans_cat" -> x.asInstanceOf[Number]).getOrElse(service_builder += "cans_cat" -> -1.longValue)
  	        (data \ "cans").asOpt[Long].map (cans => service_builder += "cans" -> cans.asInstanceOf[Number]).getOrElse(service_builder += "cans" -> -1.longValue)
            (data \ "facility").asOpt[Long].map (cans => service_builder += "facility" -> cans.asInstanceOf[Number]).getOrElse(service_builder += "facility" -> 0.intValue)
  	       
            (data \ "images").asOpt[List[String]].map { lst => 
                service_builder += "images" -> lst
            }.getOrElse(service_builder += "images" -> MongoDBList.newBuilder.result) 
  	       
            service_builder += "distinct" -> (data \ "distinct").asOpt[String].map (x => x).getOrElse("")
            service_builder += "address" -> (data \ "address").asOpt[String].map (x => x).getOrElse("")
            service_builder += "adjust_address" -> (data \ "adjust_address").asOpt[String].map (x => x).getOrElse("")
            
            val age_boundary = MongoDBObject.newBuilder
            (data \ "age_boundary").asOpt[JsValue].map { boundary => 
                age_boundary += "lsl" -> (boundary \ "lsl").asOpt[Int].map (x => x).getOrElse(3)
                age_boundary += "usl" -> (boundary \ "usl").asOpt[Int].map (x => x).getOrElse(11)
            }.getOrElse {
  	            age_boundary += "lsl" -> 3.longValue
  	            age_boundary += "usl" -> 11.longValue
            }
  	        service_builder += "age_boundary" -> age_boundary.result
           
  	        service_builder += "least_hours" -> (data \ "least_hours").asOpt[Int].map (x => x).getOrElse(0)
  	        service_builder += "allow_leave" -> (data \ "allow_leave").asOpt[Int].map (x => x).getOrElse(0)
  	        service_builder += "service_cat" -> (data \ "service_cat").asOpt[Int].map (x => x).getOrElse(0)
  	       
  	        /**
  	         * somethin that need to be modified at last
  	         */
  	        /**************************************************************/
  	        service_builder += "least_times" -> (data \ "least_times").asOpt[Int].map (x => x).getOrElse(0)
  	        service_builder += "lecture_length" -> (data \ "lecture_length").asOpt[Float].map (x => x).getOrElse(0)
  	        service_builder += "other_words" -> (data \ "other_words").asOpt[String].map (x => x).getOrElse("")
  	        /**************************************************************/
  	        
  	        service_builder += "date" -> new Date().getTime
	        service_builder += "servant_no" -> (data \ "servant_no").asOpt[Int].map (x => x).getOrElse(1)
  	        service_builder += "reserve1" -> (data \ "reserve1").asOpt[String].map (x => x).getOrElse("")
  	        
  	        _data_connection.getCollection("kidnap") += service_builder.result

  	        (Some(Map("service_id" -> toJson(service_id), "user_id" -> toJson(owner_id))), None)
  	      
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	    
  	}
  	
  	def popKidnapService(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).pop(data, origin)
  	}
  	
  	def popKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
            origin += "status" -> kidnapServiceStatus.removed.t.asInstanceOf[Number]
            _data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), origin)

//            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
			(Some(Map("service_id" -> toJson(service_id))), None)
  	      
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	}
  	
  	def updateKidnapService(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = { 
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).update(data, origin)
  	}
  	
  	def updateKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = {
       
  	    try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
           
            (data \ "title").asOpt[String].map (x => origin += "title" -> x).getOrElse(Unit)
            (data \ "description").asOpt[String].map (x => origin += "description" -> x).getOrElse(Unit)
            (data \ "capacity").asOpt[Int].map (x => origin += "capacity" -> x.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "price").asOpt[Float].map (x => origin += "price" -> x.asInstanceOf[Number]).getOrElse(Unit)

            (data \ "location").asOpt[JsValue].map { loc =>
                val location = MongoDBObject.newBuilder
  	            location += "latitude" -> (loc \ "latitude").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	            location += "longtitude" -> (loc \ "longtitude").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	            origin += "location" -> location.result            
            }.getOrElse(Unit)
            
            // (data \ "offer_date").asOpt[JsValue].map { date =>
            //     val offer_date = MongoDBObject.newBuilder
  	        //     offer_date += "start" -> (date \ "start").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	        //     offer_date += "end" -> (date \ "end").asOpt[Float].map (tmp => tmp).getOrElse(0.floatValue) 
  	        //     origin += "offer_date" -> offer_date.result            
            // }.getOrElse(Unit)

//            val offer_date = MongoDBList.newBuilder
//            (data \ "offer_date").asOpt[List[JsValue]].map { lst =>
//                lst map { date =>
//                    val one_date = MongoDBObject.newBuilder
//                    one_date += "day" -> (date \ "day").asOpt[Int].get
//
//                    val se_lst = MongoDBList.newBuilder
//                    (date \ "occurance").asOpt[List[JsValue]].get map { occ =>
//                        val se = MongoDBObject.newBuilder
//                        se += "start" -> (occ \ "start").asOpt[Int].get
//                        se += "end" -> (occ \ "end").asOpt[Int].get
//                        se_lst += se.result
//                    }
//                    one_date += "occurance" -> se_lst.result
//                    offer_date += one_date.result
//                }
//            }.getOrElse (Unit)
//            origin += "offer_date" -> offer_date.result
           
  	        (data \ "cans_cat").asOpt[Long].map (x => origin += "cans_cat" -> x.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "cans").asOpt[Long].map (cans => origin += "cans" -> cans.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "facility").asOpt[Long].map (cans => origin += "facility" -> cans.asInstanceOf[Number]).getOrElse(Unit)
  	       
            (data \ "images").asOpt[List[String]].map { lst => 
                origin += "images" -> lst
            }.getOrElse(Unit) 
            
            (data \ "distinct").asOpt[String].map (x => origin += "distinct" -> x).getOrElse(Unit)
            (data \ "address").asOpt[String].map (x => origin += "address" -> x).getOrElse(Unit)
            (data \ "adjust_address").asOpt[String].map (x => origin += "adjust_address" -> x).getOrElse(Unit)
            
            (data \ "age_boundary").asOpt[JsValue].map { boundary => 
                val age_boundary = MongoDBObject.newBuilder
                age_boundary += "lsl" -> (boundary \ "lsl").asOpt[Int].map (x => x).getOrElse(3)
                age_boundary += "usl" -> (boundary \ "usl").asOpt[Int].map (x => x).getOrElse(11)
                origin += "age_boundary" -> age_boundary.result
            }.getOrElse(Unit)

            (data \ "least_hours").asOpt[Int].map (x => origin += "least_hours" -> x.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "allow_leave").asOpt[Int].map (x => origin += "allow_leave" -> x.asInstanceOf[Number]).getOrElse(Unit)
            (data \ "service_cat").asOpt[Int].map (x => origin += "service_cat" -> x.asInstanceOf[Number]).getOrElse(Unit)
            
			/**
  	         * somethin that need to be modified at last
  	         */
  	        /**************************************************************/
            (data \ "least_times").asOpt[Int].map (x => origin += "least_times" -> x.asInstanceOf[Number]).getOrElse(Unit)
  	        (data \ "lecture_length").asOpt[Float].map (x => origin += "lecture_length" -> x.asInstanceOf[Number]).getOrElse(Unit)
  	        (data \ "other_words").asOpt[String].map (x => origin += "other_words" -> x).getOrElse(Unit)
  	        
  	        (data \ "reserve1").asOpt[String].map (x => origin += "reserve1" -> x).getOrElse(Unit)
  	        /**************************************************************/
            
            _data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), origin)

//            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
            (Some(Map("service_id" -> toJson(service_id))), None)
  	      
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	}
  	
  	def publishKidnapService(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  		try {
  			val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse { pr match {
	  			case None => throw new Exception("wrong input")
	  			case Some(r) => r.get("service_id").map (x => x.asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))).getOrElse(throw new Exception("wrong input")) 
	  		}}
	  	
	  	    val (status, origin) = this.queryServiceStatus(Some(service_id))
	  	    kidnapProp(status).publish(toJson(Map("service_id" -> service_id)), origin)
  			
  		} catch {
  	    	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  		}

  	}
  	
  	def publishKidnapServiceImpl(data : JsValue, origin : MongoDBObject) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    try {
  	        val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
  	        origin += "status" -> kidnapServiceStatus.online.t.asInstanceOf[Number]
            _data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), origin)

            val owner_id = origin.getAs[String]("owner_id").get
            
//            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
            (Some(Map("service_id" -> toJson(service_id), "user_id" -> toJson(owner_id))), None)
  	      
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	}
  	
  	def revertKidnapService(data : JsValue) :  (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    val (status, origin) = this.queryServiceStatus((data \ "service_id").asOpt[String])
  	    kidnapProp(status).revert(data, origin)
  	}
  	
  	def revertKidnapServiceImpl(data : JsValue, origin : MongoDBObject) :  (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    try {
  	        val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
  	        origin += "status" -> kidnapServiceStatus.offine.t.asInstanceOf[Number]
            _data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), origin)

//            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("service_id" -> toJson(service_id)))))
            (Some(Map("service_id" -> toJson(service_id))), None)
  	      
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	}
  	
  	def DB2JsValue(x : MongoDBObject) : Map[String, JsValue] = {
//  	    val offer_date = toJson(x.getAs[MongoDBList]("offer_date").map { x => x.toList.asInstanceOf[List[BasicDBObject]].map { one_date =>
//                            toJson(Map("day" -> toJson(one_date.get("day").asInstanceOf[Number].intValue),
//                                       "occurance" -> toJson(one_date.get("occurance").asInstanceOf[BasicDBList].toList.asInstanceOf[List[BasicDBObject]].map { occ =>
//                                            toJson(Map("start" -> toJson(occ.getAs[Number]("start").get.intValue),
//                                                       "end" -> toJson(occ.getAs[Number]("end").get.intValue)))
//                                        })
//                                    ))
//                         }}.getOrElse(Nil))

  	    if (x == null) throw new Exception("wrong input")
  	    else Map("service_id" -> toJson(x.getAs[String]("service_id").get),
  	               "title" -> toJson(x.getAs[String]("title").get),
  	               "description" -> toJson(x.getAs[String]("description").get),
  	               "capacity" -> toJson(x.getAs[Number]("capacity").get.intValue),
  	               "price" -> toJson(x.getAs[Number]("price").get.floatValue),
  	               "owner_id" -> toJson(x.getAs[String]("owner_id").get),
//  	               "offer_date" -> offer_date,
  	               "location" -> toJson(Map("latitude" -> toJson(x.getAs[MongoDBObject]("location").get.getAs[Number]("latitude").get.floatValue),
  	                                        "longtitude" -> toJson(x.getAs[MongoDBObject]("location").get.getAs[Number]("longtitude").get.floatValue))),
  	               "age_boundary" -> toJson(Map("lsl" -> toJson(x.getAs[MongoDBObject]("age_boundary").get.getAs[Number]("lsl").get.floatValue),
  	                                        "usl" -> toJson(x.getAs[MongoDBObject]("age_boundary").get.getAs[Number]("usl").get.floatValue))),
  	               "cans_cat" -> toJson(x.getAs[Number]("cans_cat").map (y => y.intValue).getOrElse(-1)),
  	               "cans" -> toJson(x.getAs[Number]("cans").get.longValue),
  	               "facility" -> toJson(x.getAs[Number]("facility").get.longValue),
  	               "date" -> toJson(x.getAs[Number]("date").get.longValue),
  	               "distinct" -> toJson(x.getAs[String]("distinct").map(x => x).getOrElse("")),
  	               "address" -> toJson(x.getAs[String]("address").get),
  	               "least_hours" -> toJson(x.getAs[Number]("least_hours").map (y => y.intValue).getOrElse(0)),
  	               "allow_leave" -> toJson(x.getAs[Number]("allow_leave").map (y => y.intValue).getOrElse(1)),
  	               "service_cat" -> toJson(x.getAs[Number]("service_cat").map (y => y.intValue).getOrElse(-1)),
  	               "adjust_address" -> toJson(x.getAs[String]("adjust_address").map (y => y).getOrElse("")),
				   "servant_no" -> toJson(x.getAs[Number]("servant_no").map (x => x.intValue).getOrElse(1)),
  	               "images" -> toJson(x.getAs[MongoDBList]("images").get.toList.asInstanceOf[List[String]]),
  	               "least_times" -> toJson(x.getAs[Number]("least_times").map (y => y.intValue).getOrElse(0)),
  	               "lecture_length" -> toJson(x.getAs[Number]("lecture_length").map (y => y.floatValue).getOrElse(0.0.asInstanceOf[Float])),
  	               "other_words" -> toJson(x.getAs[String]("other_words").map (y => y).getOrElse("")),
  	               "reserve1" -> toJson(x.getAs[String]("reserve1").map (y => y).getOrElse(""))
  	               )
    }
  
  	def queryKidnapServiceDetail(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    try {
  	        val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("service not existing"))
  	        val result = (from db() in "kidnap" where ("service_id" -> service_id) select (DB2JsValue(_))).head
      	    (Some(result), None)
  	      
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	}
  	               
  	def searchKidnapService(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  		
  		def basicConditions = DBObject("status" -> kidnapServiceStatus.online.t)
 	
  		var sortByLoc = false
 	
  		def conditionAcc(key : String, data : JsValue)
  						(f : JsValue => Option[AnyRef])(c : AnyRef => DBObject)
  						: Option[DBObject] = f(data \ key) match {
								  				case None => None
								  				case Some(x) => Some(c(x))	
  											 }

  		def serviceCatConditions = conditionAcc("service_cat", data)(x => x.asOpt[Int].asInstanceOf[Option[Number]])(y => DBObject("service_cat" -> y))
  		def cansCatConditions = conditionAcc("cans_cat", data)(x => x.asOpt[Int].asInstanceOf[Option[Number]])(y => DBObject("cans_cat" -> y))
  		def dateConditions = conditionAcc("date" , data)(x => x.asOpt[Long].asInstanceOf[Option[Number]])(y => ("date" $lte y.asInstanceOf[Number].longValue))
  		def locationConditions = conditionAcc("location" , data)(x => x.asOpt[JsValue]) { y => 
  			sortByLoc = true
  			val tmp = y.asInstanceOf[JsValue]
  			val lat = (tmp \ "latitude").asOpt[Double].map (x => x).getOrElse(throw new Exception("wrong input"))
  			val log = (tmp \ "longtitude").asOpt[Double].map (x => x).getOrElse(throw new Exception("wrong input"))
  			"location" $near (lat, log)
  		}

  		val conditions = $and((serviceCatConditions :: dateConditions :: cansCatConditions :: locationConditions :: Some(basicConditions) :: Nil) filterNot (_ == None) map (_.get))
  		
  	    try {
  	        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20.intValue)
  	        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0.intValue)
  	        val date = (data \ "date").asOpt[Long].map (x => x).getOrElse(new Date().getTime)
  	   
			val result = if (sortByLoc) (from db() in "kidnap" where conditions).selectSkipTopLoc(skip)(take)(DB2JsValue(_)).toList
						 else (from db() in "kidnap" where conditions).selectSkipTop(skip)(take)("date")(DB2JsValue(_)).toList
      	           
      	    (Some(Map("result" -> toJson(result))), None)
  	      
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	}
  	
  	def mineKidnapService(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
  	    try {
  	        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))

  	        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20.intValue)
  	        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0.intValue)
  	       
      	    val result = (from db() in "kidnap" where ("owner_id" -> owner_id)).
							selectSkipTop(skip)(take)("date")(DB2JsValue(_)).toList
      
			(Some(Map("result" -> toJson(result))), None)
  	      
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	}
  	
  	def queryMultipleService(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = 
  	    try {
  	        val lst = (data \ "lst").asOpt[List[String]].map (x => x).getOrElse(throw new Exception)
  	        
  	        def conditionsAcc(id : String, o : Option[DBObject]) : Option[DBObject] = o match {
  	              case None => Some("service_id" $eq id)
  	              case Some(x) => Some($or(x, "service_id" $eq id))
  	            }
  	        
  	        def conditions(l : List[String], o : Option[DBObject]) : Option[DBObject] = l match {
  	          case Nil => o
  	          case head :: tail => conditions(tail, conditionsAcc(head, o))
  	        }
  	        
  	        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0.intValue)
  	        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20.intValue)
  	      
  	        val reVal = conditions(lst, None) match {
  	          case None => toJson(List[String]())
  	          case Some(x) => {
  	        	  toJson((from db() in "kidnap" where x).selectSkipTop(skip)(take)("date")(DB2JsValue(_)).toList)   //select(DB2JsValue(_))).toList)
  	          }
  	        }
  
//  	        toJson(Map("status" -> toJson("ok"), "result" -> reVal))
  	        (Some(Map("result" -> reVal)), None)
  	        
  	    } catch {
  	      case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
  	    }
  	    
    def serviceResultMerge(rst : List[Map[String, JsValue]]) : Map[String, JsValue] = {

    	val a = rst.head.get("result").get.asOpt[List[JsValue]].get
    	val b = rst.tail.head.get("result").get.asOpt[List[JsValue]].get
    	val c = rst.tail.tail.head.get("result").get.asOpt[List[JsValue]].get

		import pattern.ParallelMessage.f
		val result = (a zip b zip c).map (tmp => f(tmp._1._1.as[JsObject].value.toMap :: tmp._1._2.as[JsObject].value.toMap :: tmp._2.as[JsObject].value.toMap :: Nil))
		Map("result" -> toJson(result))
    }
    
    def detailResultMerge(rst : List[Map[String, JsValue]]) : Map[String, JsValue] = {
		import pattern.ParallelMessage.f
		Map("result" -> toJson(f(rst)))
    }
    
    def paralleServiceForOrder(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
    	try {
    		val order_lst = pr match {
    			case None => throw new Exception("wrong input")
    			case Some(m) => m.get("result").map (x => x.asOpt[List[JsValue]].get).getOrElse(throw new Exception("wrong input"))
    		}
    		
    		if (order_lst.isEmpty) {
    		    (Some(Map("result" -> toJson(order_lst))), None)
    		} else {
        		val service_id_lst = order_lst.map (x => (x \ "service_id").asOpt[String].get)
        		
        		val conditions = $or(service_id_lst.map (x => DBObject("service_id" -> x)))
        		val result = (from db() in "kidnap" where conditions select (DB2JsValue(_))).toList
        		
        		val fr = order_lst map { tmp => 
        			val o = tmp.as[JsObject].value.toMap
        			val s = result.find(x => x.get("service_id").get.asOpt[String].get == o.get("service_id").get.asOpt[String].get).get
        			toJson(Map("order" -> tmp, "service" -> toJson(s)))
        		}
        		(Some(Map("message" -> toJson("service_for_order"), "result" -> toJson(fr))), None)
    		}
    		
    	} catch {
    		case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
    	}
    }
}