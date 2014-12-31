package controllers

import play.api._
import play.api.mvc._

import com.mongodb.casbah.Imports._

import play.api.libs.json._
import play.api.libs.json.Json
import play.api.libs.json.Json._

import module.login.LoginModule

object Application extends Controller {
 
	private def requestArgs(request : Request[AnyContent])(func : JsValue => JsValue) : Result = {
		try {
		  	request.body.asJson match { 
		  	  case Some(x) => Ok(func(x))
		  	  case None => BadRequest("Bad Request for input")
		  	}  		
	  	} catch {
	  	  case _ : Exception => BadRequest("Bad Request for input")
	  	} 
	}
  
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def authUpdateDetails = Action (request => this.requestArgs(request)(LoginModule.authUpdateDetails))
	def authWithPhone = Action (request => this.requestArgs(request)(LoginModule.authWithPhone))
	def authConfirm = Action  (request => this.requestArgs(request)(LoginModule.authComfirm))
	def authWithThird = Action (request => this.requestArgs(request)(LoginModule.authWithThird))
	def connectWithThird = Action (request =>this.requestArgs(request)(LoginModule.connectWithThird))
	def authCreateUserWithPhone = Action (request =>this.requestArgs(request)(LoginModule.authCreateUserWithPhone))
}