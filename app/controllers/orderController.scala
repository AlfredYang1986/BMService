package controllers

import play.api.mvc._

import module.order.{ orderModule, orderCommentsModule }

import controllers.common.requestArgsQuery.{requestArgs}

object orderController extends Controller {
    def pushOrder = Action (request => requestArgs(request)(orderModule.pushOrder))
    def popOrder = Action (request => requestArgs(request)(orderModule.popOrder))
    
    def updateOrder = Action(request => requestArgs(request)(orderModule.updateOrder))
    def queryOrder = Action(request => requestArgs(request)(orderModule.queryOrder))
    
    def queryOwnOrder = Action(request => requestArgs(request)(orderModule.queryOwnOrder))
    def queryApplyOrder = Action(request => requestArgs(request)(orderModule.queryApplyOrder))

    def accecptOrder = Action(request => requestArgs(request)(orderModule.acceptOrder))
    def rejectOrder = Action(request => requestArgs(request)(orderModule.rejectOrder))
    def accomplishOrder = Action(request => requestArgs(request)(orderModule.accomplishOrder))
    
    def pushComments = Action(request => requestArgs(request)(orderCommentsModule.pushComments))
    def updateComments = Action(request => requestArgs(request)(orderCommentsModule.updateComments))
    def popComments = Action(request => requestArgs(request)(orderCommentsModule.popComments))
    def queryComments = Action(request => requestArgs(request)(orderCommentsModule.queryComments))
}