package controllers

import play.api.mvc._

import module.order.orderModule

import controllers.common.requestArgsQuery.{requestArgs}

object orderController extends Controller {
    def pushOrder = Action (request => requestArgs(request)(orderModule.pushOrder))
    def popOrder = Action (request => requestArgs(request)(orderModule.popOrder))
    
    def updateOrder = Action(request => requestArgs(request)(orderModule.updateOrder))
}