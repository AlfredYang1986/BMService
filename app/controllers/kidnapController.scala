package controllers

import play.api.mvc._

import module.kidnap.{ kidnapModule, kidnapCollectionModule }

import controllers.common.requestArgsQuery.{requestArgs}

object kidnapController extends Controller {
  
    /**
     * service 生命周期
     * 1. push 刚创建，只有自己能看到，服务处于离线状态
     * 2. pop 将service 删除，只有离线状态并没有接受国订单的服务可以被删除
     * 3. update 只有理想状态的service能被修改
     * 4. publish 把离线的任务发布出去，发布了之后服务的所有内容全部是只读
     * 5. revert 将任务变为离线，已经订单没有处理的任务不允许离线
     */
  	def pushKidnapService = Action (request => requestArgs(request)(kidnapModule.pushKidnapService))
  	def popKidnapService = Action (request => requestArgs(request)(kidnapModule.popKidnapService))
  	def updateKidnapService = Action (request => requestArgs(request)(kidnapModule.updateKidnapService))
  	def publishKidnapService = Action (request => requestArgs(request)(kidnapModule.publishKidnapService))
  	def revertKidnapService = Action (request => requestArgs(request)(kidnapModule.revertKidnapService))

  	def searchKidnapService = Action (request => requestArgs(request)(kidnapModule.searchKidnapService))
  	def queryKidnapServiceDetail = Action (request => requestArgs(request)(kidnapModule.queryKidnapServiceDetail))
  	def mineKidnapServiceDetail = Action (request => requestArgs(request)(kidnapModule.mineKidnapService))
  	
  	def collecteKidnapService = Action (request => requestArgs(request)(kidnapCollectionModule.collectKidnapService))
  	def unCollecteKidnapService = Action (request => requestArgs(request)(kidnapCollectionModule.unCollectKidnapService))
  	def queryCollectKidnapService = Action (request => requestArgs(request)(kidnapCollectionModule.userCollectionsLst))

  	def resetDate = Action { request => 
  		
  		import util.dao.from
		import util.dao._data_connection
		import util.errorcode.ErrorCode
		import com.mongodb.casbah.Imports._
		import java.util.Date
		import play.api.libs.json.Json
		import play.api.libs.json.Json.{toJson}
		import play.api.libs.json.JsValue
  		
  		val change = (from db() in "kidnap" select (x => x)).toList
  		val count = change.length
  		val date = new Date().getTime - 3 * count
  		
  		(change zipWithIndex).map { x => 
  			val iter = x._1
  			val index = x._2
  			val service_id = iter.getAs[String]("service_id").get
  			
  			iter += "date" -> (date + index * 2).asInstanceOf[Number]
  			_data_connection.getCollection("kidnap").update(DBObject("service_id" -> service_id), iter)
  		} 
  		
  	    Ok(toJson(Map("status" -> "ok", "result" -> "success")))
  	}
}