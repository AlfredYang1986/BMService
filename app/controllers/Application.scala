package controllers

import play.api._
import play.api.mvc._

import com.mongodb.casbah.Imports._

import play.api.libs.json._
import play.api.libs.json.Json
import play.api.libs.json.Json._

import module.login.LoginModule

object Application extends Controller {
  
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def authWithPhone = Action { request =>
	  	
	  	try {
		  	request.body.asJson match { 
		  	  case Some(x) => Ok(LoginModule.authWithPhone(x))
		  	  case None => BadRequest("Bad Request for input")
		  	}  		
	  	} catch {
	  	  case _ : Exception => BadRequest("Bad Request for input")
	  	}
	}

	def authConfirm = Action  { request =>
	  
	  	try {
		  	request.body.asJson match { 
		  	  case Some(x) => Ok(LoginModule.authComfirm(x))
		  	  case None => BadRequest("Bad Request for input")
		  	}  		
	  	} catch {
	  	  case _ : Exception => BadRequest("Bad Request for input")
	  	}
	}

	def authWithThird = Action { request =>
			  	
		try {
		  	request.body.asJson match { 
		  	  case Some(x) => Ok(LoginModule.authWithThird(x))
		  	  case None => BadRequest("Bad Request for input")
		  	}  		
	  	} catch {
	  	  case _ : Exception => BadRequest("Bad Request for input")
	  	}
	}

	def connectWithThird = Action { request =>
		
		try {
		  	request.body.asJson match { 
		  	  case Some(x) => Ok(LoginModule.connectWithThird(x))
		  	  case None => BadRequest("Bad Request for input")
		  	}  		
	  	} catch {
	  	  case _ : Exception => BadRequest("Bad Request for input")
	  	}
	}
}