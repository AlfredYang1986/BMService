package controllers.v2

import play.api._
import play.api.mvc._

object AlipayController extends Controller {
	def alipaycallback = Action { Ok }
	def redirecturi	= Action { Ok }
}