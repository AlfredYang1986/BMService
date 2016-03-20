package module.post

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile

import java.util.Date
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import scala.collection.JavaConversions._

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import util.errorcode._

import com.mongodb.casbah.Imports._

import module.login.LoginModule
import module.sercurity._
import module.common.files.fop
import module.query.QueryModule
import module.common.helpOptions
import module.notification._

import akka.actor.Actor
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef

object PostModule {
    
	val ddn = Akka.system(play.api.Play.current).actorOf(Props[DDNActor])
    
//	def postContent(data : JsValue) : JsValue = {
	def postContent(data : JsValue)(cur : MongoDBObject) : JsValue = {
	 
		/**
		 * item list store in database
		 */
		def postItemList : MongoDBList = {
			val list_builder = MongoDBList.newBuilder
			(data \ "items").asOpt[Seq[JsValue]].get map { x =>
				
				val tmp = MongoDBObject.newBuilder
				tmp += "type" -> (x \ "type").asOpt[Int].get
				tmp += "name" -> (x \ "name").asOpt[String].get
				
				list_builder += tmp.result
			}
			
	    list_builder.result
		}
		
		/**
		 * tags list store in database
		 * tags is not complusory
		 */
		def postTagsList : MongoDBList = {
		  
			def checkTags(tag_name: String, tag_type: Int) = {
				(from db() in "tags" where ("content" -> tag_name, "type" -> tag_type) select ( x => x)).toList match {
				  case Nil => {
					val tag_builder = MongoDBObject.newBuilder
					tag_builder += "content" -> tag_name
					tag_builder += "type" -> tag_type
					_data_connection.getCollection("tags") += tag_builder.result
				  }
				  case head :: Nil => 
				  case _ => ???
				}
			}
		  
			val list_builder = MongoDBList.newBuilder
			(data \ "tags").asOpt[Seq[JsValue]].map { iter => iter.map { x => 
				
				val tmp = MongoDBObject.newBuilder
				val t = (x \ "type").asOpt[Int].get
				val content = (x \ "content").asOpt[String].get
				tmp += "type" -> t
				tmp += "content" -> content
				tmp += "offsetX" -> (x \ "offsetX").asOpt[Double].map(x => x).getOrElse(-1.0)
				tmp += "offsetY" -> (x \ "offsetY").asOpt[Double].map(x => x).getOrElse(-1.0)
				
				list_builder += tmp.result
				
				checkTags(content, t)
			}}.getOrElse(Unit)
			
			list_builder.result
		}
		
		/**
		 * get data from token
		 */
		val user_id = (data \ "user_id").asOpt[String].get
		val auth_token = (data \ "auth_token").asOpt[String].get
		val description = (data \ "description").asOpt[String].get
		val location = (data \ "location").asOpt[String].map(x => x).getOrElse("")
		val title = (data \ "title").asOpt[String].map(x => x).getOrElse("")
		
		/**
		 * check the token is validate or not
		 */
//		if (!LoginModule.isAuthTokenValidate(auth_token)) {
//			ErrorCode.errorToJson("auth token not valid")
//		} else if (!LoginModule.isUserExist(user_id)) {
//			ErrorCode.errorToJson("unknown user")
//		} else {
			/**
			 * save all the data to database
			 */
			val user_name = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""))).head
			val user_photo = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))).head
		  
			val builder = MongoDBObject.newBuilder
			builder += "post_id" -> Sercurity.md5Hash(user_id + user_name + Sercurity.getTimeSpanWithSeconds)
			builder += "date" -> new Date().getTime
			builder += "owner_id" -> user_id
			builder += "owner_name" -> user_name
			builder += "owner_photo" -> user_photo
			builder += "title" -> title
			builder += "description" -> description
			builder += "items" -> postItemList
			builder += "tags" -> postTagsList
			builder += "likes_count" -> 0
			builder += "likes" -> MongoDBList()
			builder += "comments_count" -> 0
			builder += "comments" -> MongoDBList()
			
			_data_connection.getCollection("posts") += builder.result
			
			/**
			 * refresh user profile table for resent
			 */
			val user_profile = (from db() in "user_profile" where ("user_id" -> user_id) select (x => x)).head
			user_profile.get("posts_count").map { x => 
				  	val tmp : Number = x.asInstanceOf[Number].intValue + 1
				  	user_profile += "posts_count" -> tmp
				}.getOrElse(Unit)
			_data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), user_profile);
				
			Json.toJson(Map("status" -> toJson("ok"), "result" -> 
							toJson(Map("post_result" -> toJson(true)))))
//		}
	}
	
	def uploadFile(data : MultipartFormData[TemporaryFile]) : JsValue = fop.uploadFile(data)
	
	def postCommnet(data : JsValue) : JsValue = {
	 
		def resentCommentCount = 2
	  
		def createComment(user_id : String, user_name : String, content : String, user_photo : String) : MongoDBObject = {
			val comment_builder = MongoDBObject.newBuilder
			comment_builder += "comment_owner_id" -> user_id
			comment_builder += "comment_owner_name" -> user_name
			comment_builder += "comment_owner_photo" -> user_photo
			comment_builder += "comment_date" -> new Date().getTime
			comment_builder += "comment_content" -> content
			
			comment_builder.result
		}
	  
		/**
		 * get arguments
		 */
		val post_id = (data \ "post_id").asOpt[String].map(x => x).getOrElse("")
		val user_id = (data \ "user_id").asOpt[String].map(x => x).getOrElse("")
		val auth_token = (data \ "auth_token").asOpt[String].map(x => x).getOrElse("")
		val content = (data \ "content").asOpt[String].map(x => x).getOrElse("")
		
		if (post_id == "" || user_id == "" || auth_token == "") ErrorCode.errorToJson("token not valid")
		else {
		   val (user_name, user_photo) = (from db() in "user_profile" where ("user_id" -> user_id) select { x => 
        	                               (x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""),
        	                               x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))
        	                           }).head
			
			/**
			 * add commment to comments table
			 */
			val ori_comments = from db() in "post_comments" where ("post_id" -> post_id) select (x => x)
			if (ori_comments.empty) {
				val comment = createComment(user_id, user_name, content, user_photo)
				
				val comment_list_builder = MongoDBList.newBuilder
				comment_list_builder += comment 
				
				val post_builder = MongoDBObject.newBuilder
				post_builder += "post_id" -> post_id
				post_builder += "comments" -> comment_list_builder.result//.sortBy{ x => x.asInstanceOf[BasicDBObject].get("comment_date").asInstanceOf[Number].longValue }.reverse
				
				_data_connection.getCollection("post_comments") += post_builder.result
			} else {
				val comment = createComment(user_id, user_name, content, user_photo)
			
				val ori = ori_comments.head
				ori.get("comments").map { x => 
				  	val xls = x.asInstanceOf[BasicDBList]
				  	xls.add(0, comment)
				}.getOrElse(Unit)
				
				_data_connection.getCollection("post_comments").update(DBObject("post_id" -> post_id), ori);
			}
			
			/**
			 * refresh main post table resent 
			 */
			val ori_posts = from db() in "posts" where ("post_id" -> post_id) select (x => x)
			if (ori_posts.empty) ErrorCode.errorToJson("post token not vaild")
			else {
				val ori_post = ori_posts.head
				ori_post.get("comments_count").map { x => 
				  	val tmp : Number = x.asInstanceOf[Number].intValue + 1
				  	ori_post += "comments_count" -> tmp
				}.getOrElse(Unit)
				
				ori_post.get("comments").map { x => 
					val new_comments = (from db() in "post_comments" where ("post_id" -> post_id)).select(x => x.get("comments").get.asInstanceOf[DBObject])
					ori_post += "comments" -> new_comments.head.asInstanceOf[BasicDBList].take(resentCommentCount)
				}.getOrElse(Unit)

				_data_connection.getCollection("posts").update(DBObject("post_id" -> post_id), ori_post);
				QueryModule.queryComments(data)
			}
		}
	}
	
	def isPush(user_id : String, post_id : String) : Boolean = {
	    (from db() in "user_push" where ("user_id" -> user_id) select { x => 
	        x.getAs[MongoDBList]("push").map (lst => lst.toList.asInstanceOf[List[String]].contains(post_id)).getOrElse(false)
	    }).toList match {
	        case Nil => false
	        case head :: Nil => head
	    }
	}

//	def postPush(data : JsValue) : JsValue = {
	def postPush(data : JsValue)(cur : MongoDBObject) : JsValue = {
	    /**
	     * get arguments
	     */
	    val post_id = (data \ "post_id").asOpt[String].get
	    val user_id = (data \ "user_id").asOpt[String].get
	    val auth_token = (data \ "auth_token").asOpt[String].get
	 
	    def createPush(uid : String, name : String, photo : String) : MongoDBObject = {
          val push_builder = MongoDBObject.newBuilder
    			
          push_builder += "push_owner_id" -> uid
    			push_builder += "push_owner_name" -> name 
    			push_builder += "push_owner_photo" -> photo
    			push_builder += "push_date" -> new Date().getTime
    			
    			push_builder.result
	    }
	    
      /**
    	 * add like to push table
    	 */
	    def addPushDatabase(name : String, photo : String) =
	        (from db() in "post_push" where ("post_id" -> post_id) select (x => x)).toList match {
	            case Nil => {
	                val like = createPush(user_id, name, photo)
        				
        				  val like_list_builder = MongoDBList.newBuilder
        				  like_list_builder += like
        				
        				  val post_builder = MongoDBObject.newBuilder
        				  post_builder += "post_id" -> post_id
        				  post_builder += "push" -> like_list_builder.result
        			
        				  _data_connection.getCollection("post_push") += post_builder.result
	            }
	            case head :: Nil => {
	                val like = createPush(user_id, name, photo)
    			
        				  val ori = head
        				  ori.get("push").map { x => 
          				  	if (!x.asInstanceOf[BasicDBList].exists(iter => iter.asInstanceOf[DBObject].getAs[String]("push_owner_id").get.equals(user_id))) {
          				  		  x.asInstanceOf[BasicDBList].add(0, like)
          				  		  _data_connection.getCollection("post_push").update(DBObject("post_id" -> post_id), ori);
          				  	}
          				  	else Unit
        				  }.getOrElse(Unit)
	            }
	            case _ => ???
	        }
	    
      /**
    	 * refresh user push post table
    	 */
	    def addUserPushDatabase =
	        (from db() in "user_push" where ("user_id" -> user_id) select (x => x)).toList match {
	            case Nil => {
	                val ul = MongoDBObject.newBuilder
          				ul += "user_id" -> user_id
          			  
          				val user_push_list = MongoDBList.newBuilder
          				user_push_list += post_id
          			
          				ul += "push" -> user_push_list.result
          				_data_connection.getCollection("user_push") += ul.result 
	            }
	            case head :: Nil => {
	                val ul = head
          				ul.get("push").map { x =>
          					if (!x.asInstanceOf[BasicDBList].exists(iter => iter.asInstanceOf[String].equals(post_id))) {
          						x.asInstanceOf[BasicDBList].add(0, post_id)
          						_data_connection.getCollection("user_push").update(DBObject("user_id" -> user_id), ul)
          					}
          					else Unit
          				}.getOrElse(Unit) 
	            }
	            case _ => ???
	        }
	  
	    def updateUserProfile(owner_id : String) = 
	        (from db() in "user_profile" where ("user_id" -> owner_id) select (x => x)).toList match {
	            case head :: Nil =>  {
	                 head.getAs[Number]("been_pushed").map { x =>
	                     head += "been_pushed" -> (x.intValue + 1).asInstanceOf[Number]
	                 }.getOrElse(head += "been_pushed" -> 1.asInstanceOf[Number])
	                
    					    _data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), head)
	            }
	            case _ => ???
	        }
	    
	    /**
				* push notifycations to the user
				*/					
			def pushNotifycations(op : MongoDBObject, name : String, photo : String) = {			
				     var content : Map[String, JsValue] = Map.empty
				     content += "type" -> toJson(module.common.AcitionType.push.index)
				     content += "sender_screen_name" -> toJson(name)
				     content += "sender_screen_photo" -> toJson(photo)
				     content += "sender_id" -> toJson(user_id)
				     content += "date" -> toJson(new Date().getTime)
				     val receiver_id = op.get("owner_id").map(x => x.asInstanceOf[String]).getOrElse("")
				     content += "receiver_id" -> toJson(receiver_id)
				     content += "receiver_screen_name" -> toJson(op.get("owner_name").map(x => x.asInstanceOf[String]).getOrElse(""))
				     content += "receiver_screen_photo" -> toJson(op.get("owner_photo").map(x => x.asInstanceOf[String]).getOrElse(""))
				     content += "post_id" -> toJson(post_id)
				     content += "post_item" -> toJson(op.get("items").map {x => 
				                   println(x)
				                   x.asInstanceOf[BasicDBList].toList.filter {iter => 
				                         println(iter)
				                         iter.asInstanceOf[BasicDBObject].get("type").asInstanceOf[Number].intValue == 1}
				                         .head.asInstanceOf[BasicDBObject].get("name").asInstanceOf[String]).getOrElse("")})
//				                   x.asInstanceOf[BasicDBList].head.asInstanceOf[BasicDBObject].get("name").asInstanceOf[String]).getOrElse(""))
      
				     ddn ! new DDNNotifyUsers("receiverType" -> toJson(0), "receiverIds" -> toJson(List(receiver_id, user_id).distinct), "isSave" -> toJson(1), 
                                      "msgType" -> toJson(0), "content" -> toJson(toJson(content).toString))
	    }

			def pushImpl = {
			    val (user_name, user_photo) = (from db() in "user_profile" where ("user_id" -> user_id) select { x => 
	                                          (x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""),
	                                           x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))
	                                      }).head
	                       
          val op = (from db() in "posts" where ("post_id" -> post_id) select (x => x)).head
          addPushDatabase(user_name, user_photo)
          addUserPushDatabase
          pushNotifycations(op, user_name, user_photo)
          updateUserProfile(op.getAs[String]("owner_id").get)
			}
			
			(from db() in "post_push" where ("post_id" -> post_id) select (x => x.getAs[MongoDBList]("push").get)).toList match {
			    case head :: Nil => {
			        if (head.toList.asInstanceOf[List[BasicDBObject]].filter(iter => iter.get("push_owner_id").asInstanceOf[String].equals(user_id)).isEmpty)
			            pushImpl
			    }
			    case Nil => pushImpl
			}
      QueryModule.queryPush(data)
	}

	def isLiked(user_id : String, post_id : String) : Boolean = {
	    (from db() in "user_likes" where ("user_id" -> user_id) select { x => 
	        x.getAs[MongoDBList]("likes").map (lst => lst.toList.asInstanceOf[List[String]].contains(post_id)).getOrElse(false)
	    }).toList match {
	        case Nil => false
	        case head :: Nil => head
	    }
	}
	
//	def postLike(data : JsValue) : JsValue = {
	def postLike(data : JsValue)(cur : MongoDBObject) : JsValue = {
	
		  def resentLikedCount = 6
	  
		  def createLike(user_id : String, user_name : String, user_photo : String) : MongoDBObject = {
    			val like_builder = MongoDBObject.newBuilder
    			like_builder += "like_owner_id" -> user_id
    			like_builder += "like_owner_name" -> user_name
    			like_builder += "like_owner_photo" -> user_photo
    			like_builder += "like_date" -> new Date().getTime
    			
    			like_builder.result
		  }
	  
  		/**
  		 * get arguments
  		 */
  		val post_id = (data \ "post_id").asOpt[String].get
  		val user_id = (data \ "user_id").asOpt[String].get
  		val auth_token = (data \ "auth_token").asOpt[String].get

      /**
    	 * add like to likes table
    	 */
  		def addLikeDatabase(user_name : String, user_photo : String) = 
  		    (from db() in "post_likes" where ("post_id" -> post_id) select (x => x)).toList match {
  		        case Nil => {
  		            val like = createLike(user_id, user_name, user_photo)
    				
        				  val like_list_builder = MongoDBList.newBuilder
        				  like_list_builder += like
        				
        				  val post_builder = MongoDBObject.newBuilder
        				  post_builder += "post_id" -> post_id
        				  post_builder += "likes" -> like_list_builder.result
        			
        				  _data_connection.getCollection("post_likes") += post_builder.result
  		        }
  		        case head :: Nil => {
      		        val like = createLike(user_id, user_name, user_photo)
        			
        				  val ori = head
        				  ori.get("likes").map { x => 
          				  	if (!x.asInstanceOf[BasicDBList].exists(iter => iter.asInstanceOf[DBObject].getAs[String]("like_owner_id").get.equals(user_id))) {
          				  		  x.asInstanceOf[BasicDBList].add(0, like)
          				  		  _data_connection.getCollection("post_likes").update(DBObject("post_id" -> post_id), ori);
          				  	}
          				  	else Unit
        				  }.getOrElse(Unit)    
  		        }
  		        case _ => ???
  		    }

  		/**
    	 * refresh user like post table
    	 */
  		def addUserLikeDatabase =
  		    (from db() in "user_likes" where ("user_id" -> user_id) select (x => x)).toList match {
  		        case Nil => {
        		      val ul = MongoDBObject.newBuilder
          				ul += "user_id" -> user_id
          			  
          				val user_like_list = MongoDBList.newBuilder
          				user_like_list += post_id
          			
          				ul += "likes" -> user_like_list.result
          				_data_connection.getCollection("user_likes") += ul.result  
  		        }
  		        case head :: Nil => {
        		      val ul = head
          				ul.get("likes").map { x =>
          					if (!x.asInstanceOf[BasicDBList].exists(iter => iter.asInstanceOf[String].equals(post_id))) {
          						x.asInstanceOf[BasicDBList].add(0, post_id)
          						_data_connection.getCollection("user_likes").update(DBObject("user_id" -> user_id), ul);
          					}
          					else Unit
          				}.getOrElse(Unit) 
  		        }
  		        case _ => ???
  		    }
  		
  		/**
			 * push notifycations to the user
			 */					
		 def likeNotifycations(op : MongoDBObject, user_name : String, user_photo : String) = {			
		     var content : Map[String, JsValue] = Map.empty
		     content += "type" -> toJson(module.common.AcitionType.like.index)
		     content += "sender_screen_name" -> toJson(user_name)
		     content += "sender_screen_photo" -> toJson(user_photo)
		     content += "sender_id" -> toJson(user_id)
		     content += "date" -> toJson(new Date().getTime)
		     val receiver_id = op.get("owner_id").map(x => x.asInstanceOf[String]).getOrElse("")
		     content += "receiver_id" -> toJson(receiver_id)
		     content += "receiver_screen_name" -> toJson(op.get("owner_name").map(x => x.asInstanceOf[String]).getOrElse(""))
		     content += "receiver_screen_photo" -> toJson(op.get("owner_photo").map(x => x.asInstanceOf[String]).getOrElse(""))
		     content += "post_id" -> toJson(post_id)
				 content += "post_item" -> toJson(op.get("items").map {x => 
				                   println(123)
				                   println(x)
				                   x.asInstanceOf[BasicDBList].toList.filter {iter =>
				                         println(456)
				                         println(iter)
				                         iter.asInstanceOf[BasicDBObject].get("type").asInstanceOf[Number].intValue == 1}
				                         .head.asInstanceOf[BasicDBObject].get("name").asInstanceOf[String]).getOrElse("")})
//				                   x.asInstanceOf[BasicDBList].head.asInstanceOf[BasicDBObject].get("name").asInstanceOf[String]).getOrElse(""))
  
		     ddn ! new DDNNotifyUsers("receiverType" -> toJson(0), "receiverIds" -> toJson(List(receiver_id, user_id).distinct), "isSave" -> toJson(1), 
                                  "msgType" -> toJson(0), "content" -> toJson(toJson(content).toString))
		 }
  	
     /**
  	  * refresh main post table resent 
  	  * not need for now
  	  */
//		 def refreshMainPostTableForResent = {
//  			val ori_posts = from db() in "posts" where ("post_id" -> post_id) select (x => x)
//  			if (ori_posts.empty) ErrorCode.errorToJson("post token not vaild")
//  			else {
//  				val ori_post = ori_posts.head
//  				ori_post.get("likes_count").map { x => 
//  				  	val tmp : Number = x.asInstanceOf[Number].intValue + 1
//  				  	ori_post += "likes_count" -> tmp
//  				}.getOrElse(Unit)
//  				
//  				ori_post.get("likes").map { x => 
//  					val new_likes = (from db() in "post_likes" where ("post_id" -> post_id)).select(x => x.get("likes").get.asInstanceOf[DBObject])
//  					ori_post += "likes" -> new_likes.head.asInstanceOf[BasicDBList].take(resentLikedCount)
//  				}.getOrElse(Unit)
//  				_data_connection.getCollection("posts").update(DBObject("post_id" -> post_id), ori_post); 
//		 }
	
		 	def updateUserProfile(owner_id : String) = 
	        (from db() in "user_profile" where ("user_id" -> owner_id) select (x => x)).toList match {
	            case head :: Nil =>  {
	                 head.getAs[Number]("been_liked").map { x =>
	                     head += "been_liked" -> (x.intValue + 1).asInstanceOf[Number]
	                 }.getOrElse(head += "been_liked" -> 1.asInstanceOf[Number])
	                
    					    _data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), head)
	            }
	            case _ => ???
	        }
		 
		 def likeImpl = {
		   	 val (user_name, user_photo) = (from db() in "user_profile" where ("user_id" -> user_id) select { x => 
                                          (x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""),
                                           x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))
                                       }).head
                     
         val op = (from db() in "posts" where ("post_id" -> post_id) select (x => x)).head			
    		 addLikeDatabase(user_name, user_photo)
      	 addUserLikeDatabase
    		 likeNotifycations(op, user_name , user_photo)
		   	 updateUserProfile(op.getAs[String]("owner_id").get)
		 }
		 
		 (from db() in "post_likes" where ("post_id" -> post_id) select (x => x.getAs[MongoDBList]("likes").get)).toList match {
			    case head :: Nil => {
			        if (head.toList.asInstanceOf[List[BasicDBObject]].filter(iter => iter.get("like_owner_id").asInstanceOf[String].equals(user_id)).isEmpty)
			            likeImpl
			    }
			    case Nil => likeImpl
			} 
		 
		  QueryModule.queryLikes(data)(cur)
	}
	
	  def postUnlike(data : JsValue)(cur : MongoDBObject) : JsValue = {
        /**
    		 * get arguments
    		 */
    		val post_id = (data \ "post_id").asOpt[String].get
    		val user_id = (data \ "user_id").asOpt[String].get
    		val auth_token = (data \ "auth_token").asOpt[String].get
	      
    		def removeLikeDatabase =
    		    (from db() in "post_likes" where ("post_id" -> post_id) select (x => x)).toList match {
    		        case head :: Nil => {
    		            val lst = head.getAs[MongoDBList]("likes").get
    		            val tmp = lst.filterNot (iter => iter.asInstanceOf[BasicDBObject].get("like_owner_id").asInstanceOf[String].equals(user_id)) 
    		            head += "likes" -> tmp
    		            
          			    _data_connection.getCollection("post_likes").update(DBObject("post_id" -> post_id), head);
    		        }
    		        case Nil => Unit
    		        case _ => ???
    		    }
    		
    		def removeUserLikeDatabase =
    		    (from db() in "user_likes" where ("user_id" -> user_id) select (x => x)) toList match {
    		        case head :: Nil => {
    		            val lst = head.getAs[MongoDBList]("likes").get
    		            val tmp = lst.filterNot (iter => iter.asInstanceOf[String].equals(post_id))
    		            head += "likes" -> tmp
    		            
          			    _data_connection.getCollection("user_likes").update(DBObject("user_id" -> user_id), head);
    		        }
    		        case Nil => Unit
    		        case _ => ???
    		    }
    	
    		def unlikeNotifycations(op : MongoDBObject, user_name : String, user_photo : String) = {			
           var content : Map[String, JsValue] = Map.empty
           content += "type" -> toJson(module.common.AcitionType.unlike.index)
           content += "sender_screen_name" -> toJson(user_name)
           content += "sender_screen_photo" -> toJson(user_photo)
           content += "sender_id" -> toJson(user_id)
           content += "date" -> toJson(new Date().getTime)
           val receiver_id = op.get("owner_id").map(x => x.asInstanceOf[String]).getOrElse("")
           content += "receiver_id" -> toJson(receiver_id)
           content += "receiver_screen_name" -> toJson(op.get("owner_name").map(x => x.asInstanceOf[String]).getOrElse(""))
           content += "receiver_screen_photo" -> toJson(op.get("owner_photo").map(x => x.asInstanceOf[String]).getOrElse(""))
           content += "post_id" -> toJson(post_id)
				   content += "post_item" -> toJson(op.get("items").map (x => 
				                   x.asInstanceOf[BasicDBList].toList.filter (iter => 
				                         iter.asInstanceOf[BasicDBObject].get("type").asInstanceOf[Number].intValue == 1)
				                         .head.asInstanceOf[BasicDBObject].get("name").asInstanceOf[String]).getOrElse(""))
//				                   x.asInstanceOf[BasicDBList].head.asInstanceOf[BasicDBObject].get("name").asInstanceOf[String]).getOrElse(""))
          
           ddn ! new DDNNotifyUsers("receiverType" -> toJson(0), "receiverIds" -> toJson(List(receiver_id, user_id).distinct), "isSave" -> toJson(1), 
                                  "msgType" -> toJson(0), "content" -> toJson(toJson(content).toString))
		    }
    		
    		def updateUserProfile(owner_id : String) = 
  	        (from db() in "user_profile" where ("user_id" -> owner_id) select (x => x)).toList match {
  	            case head :: Nil =>  {
  	                 head.getAs[Number]("been_liked").map { x =>
  	                     head += "been_liked" -> (x.intValue - 1).asInstanceOf[Number]
  	                 }.getOrElse(head += "been_liked" -> 0.asInstanceOf[Number])
  	                
      					    _data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), head)
  	            }
  	            case _ => ???
  	        }
    		
    		def unlikeImpl = {
    		   val (user_name, user_photo) = (from db() in "user_profile" where ("user_id" -> user_id) select { x => 
                                          (x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""),
                                           x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))
                                       }).head
    		    
           val op = (from db() in "posts" where ("post_id" -> post_id) select (x => x)).head			
      		 removeLikeDatabase
        	 removeUserLikeDatabase
      		 unlikeNotifycations(op, user_name , user_photo)
  		   	 updateUserProfile(op.getAs[String]("owner_id").get)
    		}
    	
    	  (from db() in "post_likes" where ("post_id" -> post_id) select (x => x.getAs[MongoDBList]("likes").get)).toList match {
			      case head :: Nil => {
			          if (!head.toList.asInstanceOf[List[BasicDBObject]].filter(iter => iter.get("like_owner_id").asInstanceOf[String].equals(user_id)).isEmpty)
			            unlikeImpl
			      }
			      case Nil => unlikeImpl
			  } 
		 
		    QueryModule.queryLikes(data)(cur)
	  }
	   
	  def postUnpush (data : JsValue)(cur : MongoDBObject) : JsValue = {
	      /**
    		 * get arguments
    		 */
    		val post_id = (data \ "post_id").asOpt[String].get
    		val user_id = (data \ "user_id").asOpt[String].get
    		val auth_token = (data \ "auth_token").asOpt[String].get
	      
    		def removePushDatabase =
    		    (from db() in "post_push" where ("post_id" -> post_id) select (x => x)).toList match {
    		        case head :: Nil => {
    		            val lst = head.getAs[MongoDBList]("push").get
    		            val tmp = lst.filterNot (iter => iter.asInstanceOf[BasicDBObject].get("push_owner_id").asInstanceOf[String].equals(user_id)) 
    		            head += "push" -> tmp
    		            
          			    _data_connection.getCollection("push_likes").update(DBObject("post_id" -> post_id), head);
    		        }
    		        case Nil => Unit
    		        case _ => ???
    		    }
    		
    		def removeUserPushDatabase =
    		    (from db() in "user_push" where ("user_id" -> user_id) select (x => x)) toList match {
    		        case head :: Nil => {
    		            val lst = head.getAs[MongoDBList]("push").get
    		            val tmp = lst.filterNot (iter => iter.asInstanceOf[String].equals(post_id))
    		            head += "push" -> tmp
    		            
          			    _data_connection.getCollection("user_push").update(DBObject("user_id" -> user_id), head);
    		        }
    		        case Nil => Unit
    		        case _ => ???
    		    }
    		
    		def unpushNotifycations(op : MongoDBObject, user_name : String, user_photo : String) = {			
           var content : Map[String, JsValue] = Map.empty
           content += "type" -> toJson(module.common.AcitionType.unlike.index)
           content += "sender_screen_name" -> toJson(user_name)
           content += "sender_screen_photo" -> toJson(user_photo)
           content += "sender_id" -> toJson(user_id)
           content += "date" -> toJson(new Date().getTime)
           val receiver_id = op.get("owner_id").map(x => x.asInstanceOf[String]).getOrElse("")
           content += "receiver_id" -> toJson(receiver_id)
           content += "receiver_screen_name" -> toJson(op.get("owner_name").map(x => x.asInstanceOf[String]).getOrElse(""))
           content += "receiver_screen_photo" -> toJson(op.get("owner_photo").map(x => x.asInstanceOf[String]).getOrElse(""))
           content += "post_id" -> toJson(post_id)
				   content += "post_item" -> toJson(op.get("items").map (x => 
				                   x.asInstanceOf[BasicDBList].toList.filter (iter => 
				                         iter.asInstanceOf[BasicDBObject].get("type").asInstanceOf[Number].intValue == 1)
				                         .head.asInstanceOf[BasicDBObject].get("name").asInstanceOf[String]).getOrElse(""))
//				                   x.asInstanceOf[BasicDBList].head.asInstanceOf[BasicDBObject].get("name").asInstanceOf[String]).getOrElse(""))
          
           ddn ! new DDNNotifyUsers("receiverType" -> toJson(0), "receiverIds" -> toJson(List(receiver_id, user_id).distinct), "isSave" -> toJson(1), 
                                  "msgType" -> toJson(0), "content" -> toJson(toJson(content).toString))
		    }
    		
    		def updateUserProfile(owner_id : String) = 
  	        (from db() in "user_profile" where ("user_id" -> owner_id) select (x => x)).toList match {
  	            case head :: Nil =>  {
  	                 head.getAs[Number]("been_pushed").map { x =>
  	                     head += "been_pushed" -> (x.intValue - 1).asInstanceOf[Number]
  	                 }.getOrElse(head += "been_pushed" -> 0.asInstanceOf[Number])
  	                
      					    _data_connection.getCollection("user_profile").update(DBObject("user_id" -> user_id), head)
  	            }
  	            case _ => ???
  	        }
    		
    		def unpushImpl = {
    		   val (user_name, user_photo) = (from db() in "user_profile" where ("user_id" -> user_id) select { x => 
                                          (x.get("screen_name").map(y => y.asInstanceOf[String]).getOrElse(""),
                                           x.get("screen_photo").map(y => y.asInstanceOf[String]).getOrElse(""))
                                       }).head
    		    
           val op = (from db() in "posts" where ("post_id" -> post_id) select (x => x)).head			
      		 removePushDatabase
        	 removeUserPushDatabase
      		 unpushNotifycations(op, user_name , user_photo)
  		   	 updateUserProfile(op.getAs[String]("owner_id").get)
    		}
	       
	      (from db() in "post_push" where ("post_id" -> post_id) select (x => x.getAs[MongoDBList]("push").get)).toList match {
			      case head :: Nil => {
			          if (!head.toList.asInstanceOf[List[BasicDBObject]].filter(iter => iter.get("push_owner_id").asInstanceOf[String].equals(user_id)).isEmpty)
			            unpushImpl
			      }
			      case Nil => unpushImpl
			  } 
		 
		    QueryModule.queryPush(data)
	  }
}