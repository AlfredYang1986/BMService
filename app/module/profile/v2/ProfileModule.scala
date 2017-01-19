package module.profile.v2

import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson
import com.mongodb.casbah.Imports._

import module.sercurity.Sercurity
import module.sms.smsModule

import util.dao.from
import util.dao._data_connection

import dongdamessages.MessageDefines
import pattern.ModuleTrait
import ProfileMessages._

import util.errorcode.ErrorCode
import java.util.Date

object ProfileModule extends ModuleTrait {
		
	def dispatchMsg(msg : MessageDefines)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = msg match {
//		case msg_CreateProfile(data) => createUserProfile(data)(pr)
		case msg_UpdateProfile(data) => updateUserProfile(data)(pr)
		case msg_UpdateProfileWithoutResult(data) => updateProfileWithoutResult(data)(pr)
		case msg_QueryProfile(data) => queryUserProfile(data)
		case msg_ChangeToServiceProvider(data) => changeServiceProvider(data)(pr)
		
		case msg_OwnerLstNamePhoto(data) => paralleOwnerLstNamePhoto(data)(pr)
		case msg_UserLstNamePhoto(data) => paralleUserLstNamePhoto(data)(pr) 
		case msg_OneOwnerNamePhoto(data) => paralleOneNamePhoto(data)(pr)
		case _ => ???
	}
	
	def createUserProfile(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		null
	}

	def updateUserProfile(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			var user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse (pr match {
				case None => throw new Exception("wrong input")
				case Some(m) => m.get("user_id").map (x => x.asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))).getOrElse(throw new Exception("wrong input"))
			})
			var auth_token = (data \ "auth_token").asOpt[String].map(x => x).getOrElse(pr match {
				case None => ""
				case Some(m) => m.get("auth_token").map (x => x.asOpt[String].map (y => y).getOrElse("")).getOrElse("")
			})
			val screen_name = (data \ "screen_name").asOpt[String].map(x => x).getOrElse(pr match {
				case None => ""
				case Some(m) => m.get("screen_name").map (x => x.asOpt[String].map (y => y).getOrElse("")).getOrElse("")
			})
			val screen_photo = (data \ "screen_photo").asOpt[String].map(x => x).getOrElse(pr match {
				case None => ""
				case Some(m) => m.get("screen_photo").map (x => x.asOpt[String].map (y => y).getOrElse("")).getOrElse("")
			})

			(from db() in "user_profile" where ("user_id" -> user_id) select (x => x)).toList match {
				case Nil => {
					val builder = MongoDBObject.newBuilder
					
					builder += "user_id" -> user_id // c_r_user_id
					builder += "screen_name" -> screen_name
					builder += "screen_photo" -> screen_photo
					builder += "isLogin" -> (data \ "isLogin").asOpt[Int].map(x => x).getOrElse(1)
					builder += "gender" -> (data \ "gender").asOpt[Int].map(x => x).getOrElse(0)
					
					builder += "school" -> (data \ "school").asOpt[String].map (x => x).getOrElse("")
					builder += "company" -> (data \ "company").asOpt[String].map (x => x).getOrElse("")
					builder += "occupation" -> (data \ "occupation").asOpt[String].map (x => x).getOrElse("")
					builder += "personal_description" -> (data \ "personal_description").asOpt[String].map (x => x).getOrElse("")
					
					builder += "is_service_provider" -> (data \ "is_service_provider").asOpt[Int].map (x => x).getOrElse(0)
	
					val coordinate = MongoDBObject.newBuilder
					coordinate += "longtitude" -> (data \ "longtitude").asOpt[Float].map(x => x).getOrElse(0.toFloat.asInstanceOf[Number])
					coordinate += "latitude" -> (data \ "latitude").asOpt[Float].map(x => x).getOrElse(0.toFloat.asInstanceOf[Number])
					builder += "coordinate" -> coordinate.result
					
					builder += "address" -> (data \ "address").asOpt[String].map (x => x).getOrElse("")
					builder += "date" -> new Date().getTime
					builder += "dob" -> (data \ "dob").asOpt[Long].map (x => x).getOrElse(0.toFloat.asInstanceOf[Number])
					builder += "about" -> (data \ "about").asOpt[String].map (x => x).getOrElse("")
					
					builder += "contact_no" -> (data \ "phoneNo").asOpt[String].map (x => x).getOrElse("")
					
					(data \ "kids").asOpt[List[JsValue]].map { lst => 
					    val kids = MongoDBList.newBuilder
					    lst foreach { tmp => 
					        val kid = MongoDBObject.newBuilder
					        kid += "gender" -> (tmp \ "gender").asOpt[Int].map (x => x).getOrElse(0)
					        kid += "dob" -> (tmp \ "dob").asOpt[Long].map (x => x).getOrElse(0.toFloat.asInstanceOf[Number])
	
					        kids += kid.result
					    }
					    builder += "kids" -> kids.result
					}.getOrElse(builder += "kids" -> MongoDBList.newBuilder.result)
					
			        val result = builder.result
					_data_connection.getCollection("user_profile") += result
	    			(Some(DB2Map(result, auth_token)), None)
				}
				case user :: Nil => {
					List("signature", "role_tag", "screen_name", "screen_photo", "about", "address", "school", "company", "occupation", "personal_description", "contact_no") foreach { x =>
						(data \ x).asOpt[String].map { value =>
							user += x -> value
						}.getOrElse(Unit)
					}
					
					List("followings_count", "followers_count", "posts_count", "friends_count", "cycle_count", "isLogin", "gender", "is_service_provider") foreach { x => 
						(data \ x).asOpt[Int].map { value =>
							user += x -> new Integer(value)
						}.getOrElse(Unit)
					}
	
					List("dob") foreach { x => 
						(data \ x).asOpt[Long].map { value =>
							user += x -> value.asInstanceOf[Number]
						}.getOrElse(Unit)
					}
				
					List("longtitude", "latitude") foreach { x => 
						(data \ x).asOpt[Float].map { value =>
							val co = user.getAs[MongoDBObject]("coordinate").get
							co += x -> x.asInstanceOf[Number]
						}.getOrElse(Unit)
					}
					
					(data \ "kids").asOpt[List[JsValue]].map { lst => 
					    val kids = MongoDBList.newBuilder
					    lst foreach { tmp => 
					        val kid = MongoDBObject.newBuilder
					        kid += "gender" -> (tmp \ "gender").asOpt[Int].map (x => x).getOrElse(0)
					        kid += "dob" -> (tmp \ "dob").asOpt[Long].map (x => x).getOrElse(0.toFloat.asInstanceOf[Number])
	
					        kids += kid.result
					    }
					    user += "kids" -> kids.result
					}.getOrElse(Unit)
			
					_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user)
	    			(Some(DB2Map(user, auth_token)), None)
				}
				case _ => ???
			}
			
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}

	def updateProfileWithoutResult(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = updateUserProfile(data)(pr) match {
			case (Some(x), None) => (pr, None)
			case (None, Some(err)) => (None, Some(err))
			case _ => ???
		}
	
	def DB2Map(obj : MongoDBObject, token : String) : Map[String, JsValue] = {
	    var has_phone = obj.getAs[String]("contact_no").map (x => x.length > 0).getOrElse(false)
	    
		val re = Map("user_id" -> toJson(obj.getAs[String]("user_id").get),
					"isLogin" -> toJson(obj.getAs[Number]("isLogin").get.intValue),
					"gender" -> toJson(obj.getAs[Number]("gender").get.intValue),
					"screen_name" -> toJson(obj.getAs[String]("screen_name").get),
					"screen_photo" -> toJson(obj.getAs[String]("screen_photo").get),
					"school" -> toJson(obj.getAs[String]("school").get),
					"company" -> toJson(obj.getAs[String]("company").get),
					"occupation" -> toJson(obj.getAs[String]("occupation").get),
					"personal_description" -> toJson(obj.getAs[String]("personal_description").get),
					"is_service_provider" -> toJson(obj.getAs[Number]("is_service_provider").get.intValue),
					"about" -> toJson(obj.getAs[String]("about").get),
					"address" -> toJson(obj.getAs[String]("address").get),
					"contact_no" -> toJson(obj.getAs[String]("contact_no").get),
					"has_phone" -> toJson(has_phone),
					"dob" -> toJson(obj.getAs[Number]("dob").get.longValue),
					"date" -> toJson(obj.getAs[Number]("date").get.longValue),
					"coordinate" -> toJson(Map(
										"longtitude" -> obj.getAs[BasicDBObject]("coordinate").get.get("longtitude").asInstanceOf[Number].floatValue,
										"latitude" -> obj.getAs[BasicDBObject]("coordinate").get.get("latitude").asInstanceOf[Number].floatValue))
					)
		if (token.isEmpty) re
		else re + ("auth_token" -> toJson(token))
	}
	
	def queryUserProfile(data : JsValue) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val query_user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse(throw new Exception("wrong input"))
			val query_auth_token = (data \ "auth_token").asOpt[String].map(x => x).getOrElse(throw new Exception("wrong input"))
			val owner_user_id = (data \ "owner_user_id").asOpt[String].map(x => x).getOrElse(throw new Exception("wrong input"))
		
			(from db() in "user_profile" where ("user_id" -> owner_user_id) select (x => x)).toList match {
				case head :: Nil => (Some(DB2Map(head, "")), None)
				case _ => throw new Exception("unknown user")
			}
			
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
	
	def changeServiceProvider(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			var user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse (pr match {
				case None => throw new Exception("wrong input")
				case Some(m) => m.get("user_id").map (x => x.asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))).getOrElse(throw new Exception("wrong input"))
			})

			(from db() in "user_profile" where ("user_id" -> user_id) select (x => x)).toList match {
				case head :: Nil => {
					if (head.getAs[Number]("is_service_provider").get.intValue == 0) {
						head += "is_service_provider" -> 1.asInstanceOf[Number]
						_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), head)
					}
					(pr, None)
				}
				case _ => throw new Exception("unknow user")
			}
			
		} catch {
			case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}
	}
	
	def paralleServiceNamePhotoImpl(data : JsValue)(pr : Option[Map[String, JsValue]])(f : JsValue => String) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val lst = pr match {
    			case None => throw new Exception("wrong input")
    			case Some(m) => m.get("result").map (x => x.asOpt[List[JsValue]].get).getOrElse(throw new Exception("wrong input"))
    		}
			
			if (lst.isEmpty) {
				val fr : List[JsValue] = Nil
	    		(Some(Map("message" -> toJson("profile_name_photo"), "result" -> toJson(fr))), None)
				
			} else {
							
	    		val owner_lst = (lst map (x => f(x)))
	    
	    		val conditions = $or(owner_lst map (x => DBObject("user_id" -> x)))
	    		val owner_profile_lst = (from db() in "user_profile" where conditions select { x => 
		    			Map("user_id" -> x.getAs[String]("user_id").get,
							"screen_name" -> x.getAs[String]("screen_name").get,
		    				"screen_photo" -> x.getAs[String]("screen_photo").get)
	    			}).toList
	    		
	            val result = owner_lst map (x => toJson(owner_profile_lst.find(y => y.get("user_id").get == x)))
	    		(Some(Map("message" -> toJson("profile_name_photo"), "result" -> toJson(result))), None)
			}
			
		} catch {
        	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}	
	}
	
	def paralleOwnerLstNamePhoto(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = paralleServiceNamePhotoImpl(data)(pr)(x =>  (x \ "owner_id").asOpt[String].get)
	def paralleUserLstNamePhoto(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = paralleServiceNamePhotoImpl(data)(pr)(x =>  (x \ "user_id").asOpt[String].get)
	
	def paralleOneNamePhoto(data : JsValue)(pr : Option[Map[String, JsValue]]) : (Option[Map[String, JsValue]], Option[JsValue]) = {
		try {
			val tmp_user_id = pr match {
				case None => throw new Exception("wrong input")
				case Some(m) => m.get("owner_id").get.asOpt[String].get
			}
		
			var result = (from db() in "user_profile" where ("user_id" -> tmp_user_id) select { x =>
				Map("screen_name" -> toJson(x.getAs[String]("screen_name").get),
					"screen_photo" -> toJson(x.getAs[String]("screen_photo").get))
			}).head
    		
    		(Some(result), None)
			
		} catch {
        	case ex : Exception => (None, Some(ErrorCode.errorToJson(ex.getMessage)))
		}	
	}
}