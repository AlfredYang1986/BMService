package module.notification

import play.api.libs.iteratee._
import play.api.libs.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import play.api.libs.json.JsValue
import play.api.libs.json.Json.{ toJson }

import util.dao._

object notifyMapping {
	/**
	 * restore the register user_id and web socket things
	 * which hold all the users that register their device to this service
	 */
	var registration_list = Map.empty[String,  Option[Concurrent.Channel[JsValue]]]
	def isUserRegister(user_id : String) : Boolean = registration_list.contains(user_id)
	def connectAnUser(user_id : String, channel : Concurrent.Channel[JsValue]) = registration_list = registration_list + (user_id -> Some(channel))
	def disconnectAnUser(user_id : String) = {
		user2tmpGroup foreach ( pair => user2tmpGroup += pair._1 -> pair._2.filter(_ != user_id))
		user2Group foreach ( pair => user2tmpGroup += pair._1 -> pair._2.filter(_ != user_id))
		
		registration_list = registration_list - user_id
	}
	
	/**
	 * map user_id to those tmp group that users in
	 * because the use is tmp group so the only store in the memories
	 * Map[sub_group_id, List[user_id]]
	 */
	var user2tmpGroup = Map.empty[String, List[String]]
	def userJoinTmpGroup(user_id : String, sub_group_id : String) : Boolean = {
		// 1. query data in the database to check the sub_group is still exist
//		if ((from db() in "groups" where ("sub_groups.sub_group_id" -> sub_group_id) select (x => x)).count != 1) {
//			user2tmpGroup -= sub_group_id
//			false
//		}
//		else {
			// TODO: check the user is connecting, send error?????need it ??
			// 2. if exist mapping them 
			val total_limits = 6
			user2tmpGroup.get(sub_group_id).map(x => user2tmpGroup += sub_group_id -> (user_id :: x)).getOrElse(user2tmpGroup += sub_group_id -> (user_id :: Nil))
			// 3. notify every one in the tmp group that a new person added
			notifyMapping.notify2Client(toJson(Map("method" -> toJson("joinTmpGroup"), "status" -> toJson("success"), 
			    "receiver" -> toJson(sub_group_id),
			    "receiver_type" -> toJson(0), /* tmp group */ 
			    "resent_users" -> toJson(user2tmpGroup.get(sub_group_id).get.take(total_limits)))))
			true
//		}
	}
	def unJoinTmpGroup(user_id : String, sub_group_id : String) =
		user2tmpGroup.get(sub_group_id).map(x => user2tmpGroup += sub_group_id -> x.filter(y => y != user_id))
	/**
	 * map user_id to those groups that they join in
	 * the groups can always notify to the user so the relationship shall keep in the database
	 * and for performance sake, some most popular ones are store in the memories in the future
	 * Map[per_group_id, List[user_id]]
	 */
	var user2Group = Map.empty[String, List[String]]		// TODO: add functionalities for per groups
	/**
	 * restore the register user_id and notify token for Apple notification service
	 * Map[user_id, List[token]]
	 */
	var user2Apn = Map.empty[String, List[String]]			// TODO: add functionalities for Apple notifications

	/**
	 * 
	 */
	def notify2Client(data : JsValue) = {
		(data \ "receiver_type").asOpt[Int].get match {
		  case 0 => /* tmp group */
		    val sub_group_id = (data \ "receiver").asOpt[String].get
		    user2tmpGroup.get((data \ "receiver").asOpt[String].get).map { lst =>
		    	registration_list.filter(x => lst.contains(x._1)).foreach {
		    		case (_, channel) => channel.foreach(_.push(data))
		    	}
		    }
		    
		  case 1 => /* user p2p */
		  	val to = (data \ "receiver").asOpt[String].get
		  	registration_list.find(x => x._1 == to).foreach {
		  		case (_, channel) => { channel.foreach(_.push(data))}
		  	}
		  	
		  case 2 => /* user group */???
		}
	}
}