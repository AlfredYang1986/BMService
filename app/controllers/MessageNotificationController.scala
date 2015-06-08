package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.JsValue
import play.api.Play.current
import module.messages._

object MessageNotificationController extends Controller {
	def registerDevice(user_id : String) = MessageNotificationModule().connectWs(user_id)
}