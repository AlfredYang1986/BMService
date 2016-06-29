package module.relationship

import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import java.util.Date

import module.common.helpOptions
import module.profile.ProfileModule
import scala.collection.JavaConversions._
import module.notification._

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{ toJson }
import play.api.libs.json.{ JsValue, JsObject, JsString, JsArray }
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef

import module.common.AcitionType._

/**
 * for con = 0: indicate no relations
 * 			 1: indicate user is following post_owner
 *     		 2: indicate user is followed by post_owner
 *        	 3: indicate user and post_owner are friends
 */
sealed abstract class relations(val con : Int)
object user2PostOwner {
  case object no_connections extends relations(0)
  case object same_person extends relations(1)
  case object following extends relations(2)
  case object followed extends relations(3)
  case object friends extends relations(4)
}

object RelationshipModule {

	val ddn = Akka.system(play.api.Play.current).actorOf(Props[DDNActor])
	val apn = Akka.system(play.api.Play.current).actorOf(Props[apnsActor])
	
//	def follow(data : JsValue) : JsValue = {
	def follow(data : JsValue)(cur : MongoDBObject) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val follow_user_id = (data \ "follow_user_id").asOpt[String].get

		def addfollowings(owner : String, follower : String) = {
			val lst = from db() in "relationship" where ("user_id" -> owner) select (x => x)

			if (lst.count == 0) {
				val builder = MongoDBObject.newBuilder
				
				val following_lst = MongoDBList.newBuilder
				following_lst += follower
				
				builder += "user_id" -> owner
				builder += "following" -> following_lst.result
				builder += "followed" -> MongoDBList.newBuilder.result
				builder += "friends" -> MongoDBList.newBuilder.result
	
				_data_connection.getCollection("relationship") += builder.result
				ProfileModule.updateFollowingCount(1, owner)
			}
			else if (lst.count == 1) {
				val user = lst.head
				val following = user.getAs[MongoDBList]("following").get

				if (!following.exists(x => x.asInstanceOf[String].equals(follower))) {
					following.add(follower)
					ProfileModule.updateFollowingCount(following.length, owner)
					
					val followed = user.getAs[MongoDBList]("followed").get
					if (followed.exists(x => x.asInstanceOf[String].equals(follower))) {
						val friends = user.getAs[MongoDBList]("friends").get
						if (!friends.exists(x => x.asInstanceOf[String].equals(follower))) {
							friends.add(follower)
							ProfileModule.updateFriendsCount(friends.length, owner)
						}
					}
					_data_connection.getCollection("relationship").update(DBObject("user_id" -> owner), user)
				}
			}
		}
	
		def addfolloweds(owner : String, followed : String) = {
			val lst = from db() in "relationship" where ("user_id" -> owner) select (x => x)

			if (lst.count == 0) {
				val builder = MongoDBObject.newBuilder
				
				val followed_lst = MongoDBList.newBuilder
				followed_lst += followed
				  
				builder += "user_id" -> owner
				builder += "following" -> MongoDBList.newBuilder.result
				builder += "followed" -> followed_lst.result
				builder += "friends" -> MongoDBList.newBuilder.result
	
				_data_connection.getCollection("relationship") += builder.result
				ProfileModule.updateFollowedCount(1, owner)
			}
			else if (lst.count == 1) {
				val user = lst.head
				val followed_lst = user.getAs[MongoDBList]("followed").get
				if (!followed_lst.exists(x => (x.asInstanceOf[String]).equals(followed))) {
					followed_lst.add(followed)
					ProfileModule.updateFollowedCount(followed_lst.length, owner)

					val following = user.getAs[MongoDBList]("following").get
					if (following.exists(x => x.asInstanceOf[String].equals(followed))) {
						val friends = user.getAs[MongoDBList]("friends").get
						if (!friends.exists(x => x.asInstanceOf[String].equals(followed))) {
							friends.add(followed)
							ProfileModule.updateFriendsCount(friends.length, owner)
						}
					}
					_data_connection.getCollection("relationship").update(DBObject("user_id" -> owner), user)
				}
			}
		}
		
		addfollowings(user_id, follow_user_id)
		addfolloweds(follow_user_id, user_id)
		/**
		 * 		senderAccount : notificatioin_account
		 *   	receiverType : 0 => User, 1 => ChatGroup, 2 => UserGroup
		 *    	receiverIds	: []
		 *      isSave : 0 => Not Save, 1 => Save
		 *      msgType : 0 => text, 3 => image, 4 => voice
		 *      content : message content
		 *      thumb : 
		 *      voiceLen : null
		 *      pushFormat :
		 *      extraData :	
		 */
		var query_users : Map[String, JsValue] = Map.empty
		query_users += "user_id" -> toJson(user_id)
		query_users += "auth_token" -> toJson(auth_token)
		query_users += "query_list" -> toJson(List(follow_user_id, user_id))
		
		(ProfileModule.multipleUserProfile(toJson(query_users))(cur) \ "result").asOpt[List[JsValue]].map { lst =>
			lst.foreach { x => 
				if ((x \ "user_id").asOpt[String].get.equals(user_id)) {
				  
					var content : Map[String, JsValue] = Map.empty
					content += "type" -> toJson(module.common.AcitionType.follow.index)
					content += "sender_screen_name" -> (x \ "screen_name")
					content += "sender_screen_photo" -> (x \ "screen_photo")
					content += "sender_id" -> toJson(user_id)
					content += "date" -> toJson(new Date().getTime)
					content += "receiver_id" -> toJson(follow_user_id)
					
//					ddn ! new DDNNotifyUsers("receiverType" -> toJson(0), "receiverIds" -> toJson(List(follow_user_id)), "isSave" -> toJson(1), 
//								"msgType" -> toJson(0), "content" -> toJson(toJson(content).toString))
		     ddn ! new DDNNotifyUsers("target_type" -> toJson("users"), "target" -> toJson(List(follow_user_id, user_id).distinct),
                                  "msg" -> toJson(Map("type" -> toJson("txt"), "msg"-> toJson(toJson(content).toString))),
                                  "from" -> toJson("dongda_master"))

				} else if ((x \ "user_id").asOpt[String].get.equals(follow_user_id)) {
				
					val status = (x \ "isLogin").asOpt[Int].get 
					if (status == -1) {
						apn ! new module.notification.apnsNotifyUsers("you have been followed by a friends", follow_user_id, module.common.AcitionType.follow.index)
					  
					} else if (status == 0) {
						// TODO: user login, by now do nothing
					}
				} 
			}
		  
		}.getOrElse(Unit)
			
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("message" -> toJson("follow user success"), 
		        "relations" -> toJson(this.relationsBetweenUserAndPostowner(user_id, follow_user_id).con)))))
//		queryMutureFollowingUsers(data)
	}

//	def unfollow(data : JsValue) : JsValue = {
	def unfollow(data : JsValue)(cur : MongoDBObject) : JsValue = {
			
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val follow_user_id = (data \ "follow_user_id").asOpt[String].get

		def removefollowings(owner : String, follower : String) = {
			val lst = from db() in "relationship" where ("user_id" -> owner) select (x => x)

			if (lst.count == 1) {
				val user = lst.head
				val following = user.getAs[MongoDBList]("following").get
				if (following.exists(x => (x.asInstanceOf[String]).equals(follower))) {
					following.remove(follower)
					ProfileModule.updateFollowingCount(following.length, owner)
					
					val followed = user.getAs[MongoDBList]("followed").get
					if (followed.exists(x => x.asInstanceOf[String].equals(follower))) {
						val friends = user.getAs[MongoDBList]("friends").get
						if (friends.exists(x => x.asInstanceOf[String].equals(follower))) {
							friends.remove(follower)
							ProfileModule.updateFriendsCount(friends.length, owner)
						}
					}
					
					_data_connection.getCollection("relationship").update(DBObject("user_id" -> owner), user)
				}
			}
		}
	
		def removefolloweds(owner : String, followed : String) = {
			val lst = from db() in "relationship" where ("user_id" -> owner) select (x => x)

			if (lst.count == 1) {
				val user = lst.head
				val followed_lst = user.getAs[MongoDBList]("followed").get
				if (followed_lst.exists(x => (x.asInstanceOf[String]).equals(followed))) {
					followed_lst.remove(followed)
					ProfileModule.updateFollowedCount(followed_lst.length, owner)
					
					val following = user.getAs[MongoDBList]("following").get
					if (following.exists(x => x.asInstanceOf[String].equals(followed))) {
						val friends = user.getAs[MongoDBList]("friends").get
						if (friends.exists(x => x.asInstanceOf[String].equals(followed))) {
							friends.remove(followed)
							ProfileModule.updateFriendsCount(friends.length, owner)
						}
					}
					_data_connection.getCollection("relationship").update(DBObject("user_id" -> owner), user)
				}
			}
		}

		removefollowings(user_id, follow_user_id)
		removefolloweds(follow_user_id, user_id)
		
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("message" -> toJson("follow user success"), 
		        "relations" -> toJson(this.relationsBetweenUserAndPostowner(user_id, follow_user_id).con)))))
//		queryMutureFollowingUsers(data)
	}

//	def queryFollowingUsers(data : JsValue) : JsValue = {
	def queryFollowingUsers(data : JsValue)(cur : MongoDBObject) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val owner_id = (data \ "owner_id").asOpt[String].get
		
		val lst = from db() in "relationship" where ("user_id" -> owner_id) select (x => x)
		var tmp : Map[String, JsValue] = Map.empty
		if (lst.count == 0) {
		  	tmp += "following" -> null
		} else if (lst.count == 1) {
//			tmp = this.queryResult(lst.head, List("following"))
			tmp = this.queryResult2(lst.head, List("following"), user_id)
		  
		} else {
			ErrorCode.errorToJson("unknown error")
		}
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
	}

//	def queryFollowedUsers(data : JsValue) : JsValue = {
	def queryFollowedUsers(data : JsValue)(cur : MongoDBObject) : JsValue = {
 
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val owner_id = (data \ "owner_id").asOpt[String].get
		
		val lst = from db() in "relationship" where ("user_id" -> owner_id) select (x => x)
		var tmp : Map[String, JsValue] = Map.empty
		if (lst.count == 0) {
		  	tmp += "followed" -> null
		} else if (lst.count == 1) {
//			tmp = this.queryResult(lst.head, List("followed"))
        tmp = this.queryResult2(lst.head, List("followed"), user_id)
		  
		} else {
			ErrorCode.errorToJson("unknown error")
		}
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
	}

	def queryMutureFollowingUsers(data : JsValue) : JsValue = {

		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val owner_id = (data \ "owner_id").asOpt[String].get
	
		val lst = from db() in "relationship" where ("user_id" -> owner_id) select (x => x)
		var tmp : Map[String, JsValue] = Map.empty
		if (lst.count == 0) {
		  	tmp += "friends" -> null
		} else if (lst.count == 1) {
//		  tmp = this.queryResult(lst.head, List("friends"))
		  tmp = this.queryResult2(lst.head, List("friends"), user_id)
		  
		} else {
			ErrorCode.errorToJson("unknown error")
		}
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(tmp)))
	}
	
	def queryResult(one : MongoDBObject, lst : List[String]) : Map[String, JsValue] = {
		var tmp : Map[String, JsValue] = Map.empty
		lst map { iter => 
			var tmp_lst : List[String] = Nil
		  	one.getAs[MongoDBList](iter).map (value => 
		  		value.foreach (x => tmp_lst = x.asInstanceOf[String] :: tmp_lst)).
		  		getOrElse(Unit)
		  	
		  	tmp += iter -> toJson(tmp_lst)
		}
		tmp
	}
	
	def queryResult2(one : MongoDBObject, lst : List[String], user_id : String) : Map[String, JsValue] = {
	  var tmp : Map[String, JsValue] = Map.empty
		lst map { iter => 
		  	(one.getAs[MongoDBList](iter).map { value => 
		  		value.toList.asInstanceOf[List[String]].map { x =>
		  		    toJson(Map("user_id" -> toJson(x),
                    "relations" -> toJson(relationsBetweenUserAndPostowner(user_id, x).con)))
		  	}}.getOrElse(Nil).asInstanceOf[List[JsValue]]) match {
		  	    case Nil => Unit
		  	    case x : List[JsValue] => tmp += iter -> toJson(x)
		  	}
		}
		tmp
	}

	def relationsBetweenUserAndPostowner(user_id : String, post_owner_id : String) : relations = {
		if (user_id.equals(post_owner_id)) user2PostOwner.same_person
		else {
			val lst = from db() in "relationship" where ("user_id" -> user_id) select (x => x)
			if (lst.count == 0 || lst.count > 1) user2PostOwner.no_connections
			else {
				val is_followed = lst.head.getAs[MongoDBList]("followed").get.exists(x => (x.asInstanceOf[String]).equals(post_owner_id))
				val is_following = lst.head.getAs[MongoDBList]("following").get.exists(x => (x.asInstanceOf[String]).equals(post_owner_id))
				
				if (is_followed && is_following) user2PostOwner.friends
				else if (is_following) user2PostOwner.following
				else if (is_followed) user2PostOwner.followed
				else user2PostOwner.no_connections
			}	  
		}
	} 
	
	def queryRelationsBetweenUsers(data : JsValue) : JsValue = {
		
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val owner_id = (data \ "owner_id").asOpt[String].get
		
		val result = this.relationsBetweenUserAndPostowner(user_id, owner_id)
		
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson(result.con)))
	} 
	
//	def askPhoneAddressBookFriendsJoin(data : JsValue) : JsValue = {
	def askPhoneAddressBookFriendsJoin(data : JsValue)(cur : MongoDBObject) : JsValue = {
	 
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		
		val who = (data \ "screen_name").asOpt[String].get
		val phoneNo = (data \ "phoneNo").asOpt[String].get
	
		/**
		 * send code to the phone
		 */	
		import play.api.Play.current
		import module.sms.smsModule
		smsModule().sendInvitation(phoneNo, who)
		Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("Send invatation success")))
	}
}