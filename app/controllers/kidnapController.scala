package controllers

import play.api.mvc._

import module.groups.GroupModule
import module.groups.GroupModule2

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
  	def pushKidnapService = Action (request => requestArgs(request)(GroupModule2.createChatGroup))
  	def popKidnapService = Action (request => requestArgs(request)(GroupModule2.createChatGroup))
  	def updateKidnapService = Action (request => requestArgs(request)(GroupModule2.createChatGroup))
  	def publishKidnapService = Action (request => requestArgs(request)(GroupModule2.createChatGroup))
  	def revertKidnapService = Action (request => requestArgs(request)(GroupModule2.createChatGroup))

  	def searchKidnapService = Action (request => requestArgs(request)(GroupModule2.createChatGroup))
  	def queryKidnapServiceDetail = Action (request => requestArgs(request)(GroupModule2.createChatGroup))
}