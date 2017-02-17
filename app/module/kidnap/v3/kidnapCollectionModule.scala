package module.kidnap.v3

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import pattern.ModuleTrait

import dongdamessages.MessageDefines
import kidnapCollectionMessages._

object kidnapCollectionModule extends ModuleTrait {
	
	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
		case msg_CollectionService(data) => collectKidnapService(data)
		case msg_UncollectionService(data) => unCollectKidnapService(data)
		case msg_UserCollectionLst(data) => userCollectionsLst(data)
		case msg_IsUserCollectLst(data) => isCollectiontLst(data)(pr)
		case msg_IsUserCollect(data) => isCollection(data)(pr)
		case _ => ???
	}
	
    def collectKidnapService(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = 
        try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            def pushUserService : Boolean = 
                (from db() in "user_service" where ("user_id" -> user_id) select (x => x)).toList match {  
                  case Nil => {
                      val builder = MongoDBObject.newBuilder
                      builder += "user_id" -> user_id
                      builder += "services" -> (service_id :: Nil)
                      
                      _data_connection.getCollection("user_service") += builder.result
                      true
                  }
                  case head :: Nil => {
                      val service_lst = head.getAs[MongoDBList]("services").get.toList.asInstanceOf[List[String]]
                      head += "services" -> (service_id :: service_lst).distinct 
                      _data_connection.getCollection("user_service").update(DBObject("user_id" -> user_id), head)
                      true
                  }
                  case _ => ???
                }
            
            def pushServiceUser : Boolean = 
                (from db() in "service_user" where ("service_id" -> service_id) select (x => x)).toList match {  
                  case Nil => {
                      val builder = MongoDBObject.newBuilder
                      builder += "service_id" -> service_id
                      builder += "users" -> (user_id :: Nil)
                      
                      _data_connection.getCollection("service_user") += builder.result
                      true
                  }
                  case head :: Nil => {
                      val user_lst = head.getAs[MongoDBList]("users").get.toList.asInstanceOf[List[String]]
                      head += "users" -> (user_id :: user_lst).distinct
                      _data_connection.getCollection("service_user").update(DBObject("service_id" -> service_id), head)
                      true
                  }
                  case _ => ???
                }
         
//            if (pushUserService && pushServiceUser) toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
            if (pushUserService && pushServiceUser) (Some(Map("result" -> toJson("success"))), None)
            else throw new Exception("something wrong")    
            
        } catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
    
    def unCollectKidnapService(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = 
        try {
            val service_id = (data \ "service_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
           
            def popUserService : Boolean = 
                (from db() in "user_service" where ("user_id" -> user_id) select (x => x)).toList match {
                  case Nil => throw new Exception("wrong input")
                  case head :: Nil => {
                      head += "services" -> head.getAs[MongoDBList]("services").get.toList.asInstanceOf[List[String]].filterNot (service_id.equals(_))
                      _data_connection.getCollection("user_service").update(DBObject("user_id" -> user_id), head)
                      true
                  }
                  case _ => ???
                }
            
            def popServiceUser : Boolean = {
                (from db() in "service_user" where ("service_id" -> service_id) select (x => x)).toList match {
                  case Nil => throw new Exception("wrong input")
                  case head :: Nil => {
                      head += "users" -> head.getAs[MongoDBList]("users").get.toList.asInstanceOf[List[String]].filterNot (user_id.equals(_))
                      _data_connection.getCollection("service_user").update(DBObject("service_id" -> service_id), head)
                      true                   
                  }
                  case _ => ???
                }
            }
            
//            if (popUserService && popServiceUser) toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
            if (popUserService && popServiceUser) (Some(Map("result" -> toJson("success"))), None)
            else throw new Exception("something wrong")
            
        } catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
        
    def userCollectionsLst(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = 
        try {
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val lst = (from db() in "user_service" where ("user_id" -> user_id) select (x => 
                          x.getAs[MongoDBList]("services").get.toList.asInstanceOf[List[String]])).toList
                        
            lst match {
//              case Nil => toJson(Map("status" -> toJson("ok"), "result" -> toJson(List[String]())))
              case Nil => (Some(Map("result" -> toJson(List.empty[String]))), None)
//              case head :: Nil => toJson(Map("status" -> toJson("ok"), "result" -> toJson((kidnapModule.queryMultipleService(toJson(Map("lst" -> head))))._1.get)))
              case head :: Nil => (Some(kidnapModule.queryMultipleService(toJson(Map("lst" -> head)))._1.get), None)
              case _ => throw new Exception("wrong input")
            }
        } catch {
          case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
        }
        
	def isCollectiontLst(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val lst = pr match {
    			case None => throw new Exception("wrong input")
    			case Some(m) => m.get("result").map (x => x.asOpt[List[JsValue]].get).getOrElse(throw new Exception("wrong input"))
    		}

    		val service_lst = (lst map (x => (x \ "service_id").asOpt[String].get))

    		val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val collect_lst = (from db() in "user_service" where ("user_id" -> user_id) select (x =>
                          x.getAs[MongoDBList]("services").get.toList.asInstanceOf[List[String]])).toList.flatten

            val result = service_lst map { x =>
            	toJson(Map("service_id" -> toJson(x),
	            	"iscollect" -> toJson(collect_lst.contains(x))))
            }

    		(Some(Map("message" -> toJson("collections_lst"), "result" -> toJson(result))), None)
			
		} catch {
        	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
	
	def isCollection(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
			val tmp_service_id = pr match {
				case None => throw new Exception("wrong input")
				case Some(m) => m.get("service_id").get.asOpt[String].get
			}
			
            val collect_lst = (from db() in "user_service" where ("user_id" -> user_id) select (x => 
                          x.getAs[MongoDBList]("services").get.toList.asInstanceOf[List[String]])).toList.flatten
			val is_collect = collect_lst.contains(tmp_service_id)
			
    		(Some(Map("service_id" -> toJson(tmp_service_id), "iscollect" -> toJson(is_collect))), None)
			
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
}