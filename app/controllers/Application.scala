package controllers

import play.api._
import play.api.mvc._

import com.mongodb.casbah.Imports._

import play.api.libs.json._
import play.api.libs.json.Json
import play.api.libs.json.Json._

object Application extends Controller {
  
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def authWithPhone = Action { request =>
	  	request.body.asJson map { json => 
	  	  	(json \ "phoneNo").asOpt[String].map (x => println(x))
	  	}
	  	
	  	Ok(<message>Alfred Test</message>)
	}

	def authConfirm = Action  {
		Ok("auth confirm")
	}

	def authWithThird = Action {
		Ok("auth with third")
	}

	def connectWithThird = Action {
		Ok("connect with third")
	}
}