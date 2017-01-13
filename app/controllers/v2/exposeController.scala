package controllers.v2

import play.api.mvc._
import module.expose.ExposeModule
import controllers.common.requestArgsQuery.{requestArgs}

object exposeController extends Controller {
	  def expostUser = Action (request => requestArgs(request)(ExposeModule.expose))
	  def expostContent = Action (request => requestArgs(request)(ExposeModule.expose))
}