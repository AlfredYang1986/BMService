package module.login

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import module.sms._
import module.relationship._
import module.profile.ProfileModule

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import akka.util.Timeout
import scala.concurrent.duration._

object LoginModule {
  
	def isAuthTokenValidate(token : String) : Boolean = !((from db() in "users" where ("auth_token" -> token) select (x => x)).empty)
	def isUserExist(user_id : String) : Boolean = !((from db() in "users" where ("user_id" -> user_id) select (x => x)).empty)
	
	def authUpdateDetails(data : JsValue) : JsValue = {
		val auth_token = (data \ "auth_token").asOpt[String].get
		val user_id= (data \ "user_id").asOpt[String].get
	
		val rel = from db() in "users" where ("user_id" -> user_id) select (x => x)
		if (rel.empty) ErrorCode.errorToJson("auth token not valid")
		else {
			val user = rel.head
			List("name", "phoneNo", "email", "pwd") foreach { x => 
//			List("phoneNo", "email") foreach { x => 
				(data \ x).asOpt[String].map { value =>
				  user += x -> value
				}.getOrElse(Unit)
			}
			_data_connection.getCollection("users").update(DBObject("user_id" -> user_id), user)
			
			Json.toJson(Map("status" -> toJson("ok")))
		}
	}
  
	def authWithPhone(data : JsValue) : JsValue = {
	
		val phoneNo = (data \ "phoneNo").asOpt[String].get
		
		/**
		 * generate code
		 */
		val code = 11111 // fake one
//		val code = scala.util.Random.nextInt(90000) + 10000

		/**
		 * generate a reg token
		 */
		val time_span_minutes = Sercurity.getTimeSpanWith10Minutes
		val reg_token = Sercurity.md5Hash(phoneNo + time_span_minutes)
		
		val builder = MongoDBObject.newBuilder
		builder += "phoneNo" -> phoneNo
		builder += "code" -> code
		builder += "reg_token" -> reg_token
		
		val rel = from db() in "reg" where ("phoneNo" -> phoneNo) select (x => x) 
		if (rel.empty) _data_connection.getCollection("reg") += builder.result
		else _data_connection.getCollection("reg").update(DBObject("phoneNo" -> phoneNo), builder.result)

		/**
		 * send code to the phone
		 */	
//		import play.api.Play.current
//		smsModule().sendSMS(phoneNo, code.toString)
		
		/**
		 * return 
		 */
		Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("reg_token" -> toJson(reg_token), "phoneNo" -> toJson(phoneNo)))))
	}

	def authComfirm(data : JsValue) : JsValue = {

		val phoneNo = (data \ "phoneNo").asOpt[String].get
		val code = (data \ "code").asOpt[String].get.toInt
		val reg_token = (data \ "reg_token").asOpt[String].get
		
		val time_span_minutes = Sercurity.getTimeSpanWith10Minutes
		val reg_token_new = Sercurity.md5Hash(phoneNo + time_span_minutes)
		
		if (!reg_token_new.equals(reg_token)) {
			ErrorCode.errorToJson("token exprie")
		} else {
			val rel = from db() in "reg" where ("phoneNo" -> phoneNo) select (x => x)
			if (rel.empty) 
				ErrorCode.errorToJson("phone number not valid")
			else {
				if (code != rel.head.get("code").get.asInstanceOf[Int])
					ErrorCode.errorToJson("wrong validation code")
				else {
					/**
					 * when login success save user to the client database
					 */
					var result = from db() in "users" where ("phoneNo" -> phoneNo) select (x => x)
					if (result.empty) {
						
						/**
						 * 1. this phone is not reg
						 * 		create a new auth_token and connect to this phone number
						 */
						this.authCreateNewUserWithPhone(phoneNo)

					} else {
					  	/**
						 * 2. this phone is already reg
						 * 		pass this token to the client
						 */
						val cur = result.head
					  
						val auth_token = cur.get("auth_token").get.asInstanceOf[String]
						val user_id = cur.get("user_id").get.asInstanceOf[String]

						var tmp = ProfileModule.queryUserProfile(user_id)
						if (tmp == null) {
							tmp = ProfileModule.creatUserProfile(user_id, phoneNo)
						
							tmp += "message" -> toJson("new user")		// phone is already reg
							tmp += "phoneNo" -> toJson(phoneNo)
							tmp += "auth_token" -> toJson(auth_token)
						} else {
							tmp += "message" -> toJson("already login")		// phone is already reg
							tmp += "phoneNo" -> toJson(phoneNo)
							tmp += "auth_token" -> toJson(auth_token)
						}
					
						Json.toJson(Map("status" -> toJson("error"), "error" -> toJson(tmp))) 
					}
				} 
			}
		}
	}
	
	private def createNewUserWithProviderDetails(provide_name: String, provide_token: String, provide_uid: String, provide_screen_name: String, provide_screen_photo : String) : JsValue = {
		val new_builder = MongoDBObject.newBuilder
		
		val time_span = Sercurity.getTimeSpanWithMillSeconds
		val user_id = Sercurity.md5Hash(provide_name + provide_token + time_span)
		val auth_token = Sercurity.md5Hash(provide_token + provide_name + time_span)
					
		new_builder  += "user_id" -> user_id
		new_builder  += "auth_token" -> auth_token
		new_builder  += "phoneNo" -> ""
		new_builder  += "email" -> ""
		new_builder  += "name" -> provide_screen_name
		new_builder  += "pwd" -> "12345"
		
		val new_third_builder = MongoDBList.newBuilder

		val builder_third = MongoDBObject.newBuilder
		builder_third += ("provide_name") -> provide_name
		builder_third += ("provide_token") -> provide_token
		builder_third += ("provide_uid") -> provide_uid
		builder_third += ("provide_screen_name") -> provide_screen_name
		builder_third += ("provide_screen_photo") -> provide_screen_photo

		new_third_builder += builder_third.result
		
		new_builder  += "third" -> new_third_builder.result
	 
		_data_connection.getCollection("users") += new_builder.result
		
		ProfileModule.updateUserProfile(Json.toJson(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token),
		        "screen_name" -> toJson(provide_screen_name), "screen_photo" -> toJson(provide_screen_photo), "isLogin" -> toJson(1))))
		
//		Json.toJson(Map("status" -> toJson("ok"), "result" -> 
//				toJson(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token), "screen_name" -> toJson(provide_screen_name), "screen_photo" -> toJson(provide_screen_photo)))))
	}

	private def connectUserWithProviderDetails(user: MongoDBObject, provide_name: String, provide_token: String, provide_uid: String, provide_screen_name: String, provide_screen_photo : String) : JsValue = {

		val auth_token = user.get("auth_token").get.asInstanceOf[String]
		val user_id = user.get("user_id").get.asInstanceOf[String]
		val third_list = user.get("third").get.asInstanceOf[BasicDBList]
		var name = user.get("name").get.asInstanceOf[String]
		
		if (name == "") {
			name = provide_name
			user += ("name") -> name
		}
		val tmp = third_list.find(x => x.asInstanceOf[BasicDBObject].get("provide_name") ==  provide_name)
			
		tmp match {
			case Some(x) => {
				  x.asInstanceOf[BasicDBObject] += ("provide_name") -> provide_name
				  x.asInstanceOf[BasicDBObject] += ("provide_token") -> provide_token
				  x.asInstanceOf[BasicDBObject] += ("provide_uid") -> provide_uid
				  x.asInstanceOf[BasicDBObject] += ("provide_screen_name") -> provide_screen_name
				  x.asInstanceOf[BasicDBObject] += ("provide_screen_photo") -> provide_screen_photo

				  _data_connection.getCollection("users").update(DBObject("auth_token" -> auth_token), user)
			}
			
			case None => {
				  val builder = MongoDBObject.newBuilder
			    
				  builder += ("provide_name") -> provide_name
				  builder += ("provide_token") -> provide_token
				  builder += ("provide_uid") -> provide_uid
				  builder += ("provide_screen_name") -> provide_screen_name
				  builder += ("provide_screen_photo") -> provide_screen_photo
				  third_list += builder.result

				  _data_connection.getCollection("users").update(DBObject("auth_token" -> auth_token), user)
			}
		}
		
		ProfileModule.updateUserProfile(Json.toJson(Map("user_id" -> toJson(user_id), "auth_token"-> toJson(auth_token), "connect_result" -> toJson("success"),
		        "screen_name" -> toJson(provide_screen_name), "screen_photo" -> toJson(provide_screen_photo), "isLogin" -> toJson(1), "isThird" -> toJson(1))))
		
//		Json.toJson(Map("status" -> toJson("ok"), "result" -> 
//				toJson(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token), "name" -> toJson(provide_screen_name), "screen_photo" -> toJson(provide_screen_photo), "connect_result" -> toJson("success")))))
	}
	
	def authWithThird(data : JsValue) : JsValue = {

//		val user_id = (data \ "user_id").asOpt[String].get
//		val auth_token = (data \ "auth_token").asOpt[String].get
		val provide_name = (data \ "provide_name").asOpt[String].get
		val provide_token = (data \ "provide_token").asOpt[String].get
		val provide_uid = (data \ "provide_uid").asOpt[String].get
		val provide_screen_name = (data \ "provide_screen_name").asOpt[String].get
		val provide_screen_photo = (data \ "provide_screen_photo").asOpt[String].get
  
		val users = from db() in "users" where ("third.provide_name" -> provide_name, "third.provide_uid" -> provide_uid) select (x => x)
		
		if (users.empty) this.createNewUserWithProviderDetails(provide_name, provide_token, provide_uid, provide_screen_name, provide_screen_photo)
		else  this.connectUserWithProviderDetails(users.head, provide_name, provide_token, provide_uid, provide_screen_name, provide_screen_photo)
	}

	def connectWithThird(data : JsValue) : JsValue = authWithThird(data)
	
	def authCreateUserWithPhone(data : JsValue) : JsValue = {

	  val phoneNo = (data \ "phoneNo").asOpt[String].get

		val users = from db() in "users" where ("phoneNo" -> phoneNo) select (x => x)
		if (users.empty) {
			/**
			 * 2. if phoneNo is not, then create one directly
			 */
		  	authCreateNewUserWithPhone(phoneNo)
			
		} else {
			/**
			 * 1. if phoneNo is already connect to the auth token
			 * 		unbind the auth token
			 *   	then create new one
			 */	  
			val user = users.head
			user += "phoneNo" -> ""
			_data_connection.getCollection("users").update(DBObject("phoneNo" -> phoneNo), user)
		
			this.authCreateNewUserWithPhone(phoneNo)
		}
	}
	
	private def authCreateNewUserWithPhone(phoneNo : String) : JsValue = {
		val new_builder = MongoDBObject.newBuilder

		val time_span = Sercurity.getTimeSpanWithMillSeconds
		val user_id = Sercurity.md5Hash(phoneNo + time_span)
		val auth_token = Sercurity.md5Hash(user_id + time_span)

		new_builder  += "user_id" -> user_id
		new_builder  += "auth_token" -> auth_token
		new_builder  += "phoneNo" -> phoneNo
		new_builder  += "pwd" -> "12345"
		new_builder  += "email" -> ""
		new_builder  += "name" -> ""
						
		val new_third_builder = MongoDBList.newBuilder
		new_builder  += "third" -> new_third_builder.result
					
		_data_connection.getCollection("users") += new_builder.result
		
		var tmp = ProfileModule.queryUserProfile(user_id)
	
		if (tmp == null) {
			tmp = ProfileModule.creatUserProfile(user_id, phoneNo)
			tmp += "phoneNo" -> toJson(phoneNo)
			tmp += "auth_token" -> toJson(auth_token)
		  
		} else {
			tmp += "phoneNo" -> toJson(phoneNo)
			tmp += "auth_token" -> toJson(auth_token)
		}

		ProfileModule.updateUserProfile(Json.toJson(Map("user_id" -> toJson(user_id), "isLogin" -> toJson(1))))
		
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
	}
	
	def authWithPwd(data : JsValue) : JsValue = {
		val phoneNo = (data \ "phoneNo").asOpt[String].get
		val pwd = (data \ "pwd").asOpt[String].get
		
		val result = from db() in "users" where ("phoneNo" -> phoneNo, "pwd" -> pwd) select (x => x)
		if (result.empty) ErrorCode.errorToJson("user not existing")
		else {
			val ral = result.head
			val user_id = ral.getAs[String]("user_id").map(x => x).getOrElse(throw new Exception)
			val auth_token = ral.getAs[String]("auth_token").map(x => x).getOrElse(throw new Exception)
			val name = ral.getAs[String]("name").map(x => x).getOrElse(throw new Exception)
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
				toJson(Map("user_id" -> toJson(user_id), "auth_token" -> toJson(auth_token), "name" -> toJson(name)))))
		}
	}

	/**
	 * user status
	 * 		-1 	=> offline
	 *   	0	=> logout
	 *    	1	=> online 
	 */
	def userOffline(data : JsValue) : JsValue = {
	 	val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		ProfileModule.updateUserProfile(Json.toJson(Map("user_id" -> toJson(user_id), "isLogin" -> toJson(-1))))
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("offline success"))) 
	}
	
	def userOnline(data : JsValue) : JsValue = {
	 	val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		ProfileModule.updateUserProfile(Json.toJson(Map("user_id" -> toJson(user_id), "isLogin" -> toJson(1))))
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("online success"))) 
	}
	
	def logout(data : JsValue) : JsValue = {
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val device_token = (data \ "device_token").asOpt[String].map (x => x).getOrElse("")
		
		ProfileModule.updateUserProfile(Json.toJson(Map("user_id" -> toJson(user_id), "isLogin" -> toJson(0))))
	
		if (!device_token.equals("")) module.notification.apnsNotification.unRegisterUserDevices(user_id, device_token)

		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("logout success")))
	}
	
	/**
	 * user lst is in the sys with provider name
	 */
	def userLstInSystem(data : JsValue) : JsValue = {
	    
	    val user_id = (data \ "user_id").asOpt[String].get
	    val auth_token = (data \ "auth_token").asOpt[String].get
	    val user_lst = (data \ "lst").asOpt[List[String]].get
	   
	    def phoneConditionAcc(para : String) = "phoneNo" $eq para
	    def weiboConditionAcc(para : String) = $and("third.provider_name" $eq "weibo", "third.provider_uid" $eq para)
	    def wechatConditionAcc(para : String) = $and("third.provider_name" $eq "wechat", "third.provider_uid" $eq para)
	    def qqConditionAcc(para : String) = $and("third.provider_name" $eq "qq", "third.provider_uid" $eq para)
	   
	    def matchConditions(pn : String) : String => DBObject = pn match {
	          case "phone" => phoneConditionAcc
	          case "weibo" => weiboConditionAcc
	          case "wechat" => wechatConditionAcc
	          case "qq" => qqConditionAcc
	          case _ => ???
	        }
	    
	    val conditions : List[String] => DBObject = (data \ "provider_name").asOpt[String].map { x => { 
	        lst : List[String] => {
	          var result : DBObject = null
	          lst foreach ( iter => if (result == null) result = matchConditions(x)(iter)
                  	              else result = $or(matchConditions(x)(iter), result))
	          result
	        }
	    }}.getOrElse(lst => null)
	   
	    def userLstInSystemAcc(lst : List[String]) : List[JsValue] = {
          var fc : DBObject = null
          var result : List[JsValue] = Nil
          (from db() in "users" where conditions(lst) select (x => x/*.getAs[String]("user_id").get*/)).toList foreach { y =>
	            val id = y.getAs[String]("user_id").get
                
	            if (fc == null) fc = ("user_id" $eq id)
              else $or("user_id" $eq id, fc)
                
              result = toJson(Map("user_id" -> toJson(id), "phoneNo" -> toJson(y.getAs[String]("phoneNo")))) :: result
          }
          println(result)
           
	        val result2 = (from db() in "user_profile" where fc select { x => {
	            val id = x.getAs[String]("user_id").get
	            toJson(Map(
                  "user_id" -> toJson(id),
                  "screen_name" -> toJson(x.getAs[String]("screen_name").get),
                  "screen_photo" -> toJson(x.getAs[String]("screen_photo").get),
                  "role_tag" -> toJson(x.getAs[String]("role_tag").get),
                  "relations" -> toJson(RelationshipModule.relationsBetweenUserAndPostowner(user_id, id).con)
              ))}}).toList
              
          println(result2)
            
	        ((result.sortBy (x => (x \ "user_id").asOpt[String].get)) zip (result2.sortBy (x => (x \ "user_id").asOpt[String].get))) map { x =>
	            println(x)
	            toJson(Map(
                  "user_id" -> toJson((x._2 \ "user_id").asOpt[String].get),
                  "screen_name" -> toJson((x._2 \ "screen_name").asOpt[String].get),
                  "screen_photo" -> toJson((x._2 \ "screen_photo").asOpt[String].get),
                  "relations" -> toJson((x._2 \ "relations").asOpt[Int].get),
                  "role_tag" -> toJson((x._2 \ "role_tag").asOpt[String].get),
	                "phoneNo" -> toJson((x._1 \ "phoneNo").asOpt[String].get)
  	          ))}
      }
	    
	    (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
	        case Nil => ErrorCode.errorToJson("user not existing")
	        case head :: Nil => {
	            val iter = user_lst.distinct.grouped(20)
	            var result : List[Future[List[JsValue]]] = Nil
	            iter foreach { x => result = Future(userLstInSystemAcc(x)) :: result }
	            
	            var result0 : List[List[JsValue]] = Nil
	            result foreach { x => result0 = Await.result (x map {y => println(y); y}, Timeout(1 second).duration).asInstanceOf[List[JsValue]] :: result0 }
	            
  	          toJson(Map("status" -> toJson("ok"), "result" -> toJson(result0.filterNot(_.isEmpty) flatMap(x => x))))
	        }
	        case _ => ???
	    }
	}
}