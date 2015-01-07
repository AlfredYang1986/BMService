package controllers

import play.api._
import play.api.mvc._
import com.mongodb.casbah.Imports._
import play.api.libs.json._
import play.api.libs.json.Json
import play.api.libs.json.Json._
import module.post.PostModule
import play.api.libs.Files.TemporaryFile

object postController extends Controller {

  	private def requestArgs(request : Request[AnyContent])(func : JsValue => JsValue) : Result = {
  		try {
  			request.body.asJson.map { x => 
  				Ok(func(x))
  			}.getOrElse (BadRequest("Bad Request for input"))
  		} catch {
  			case _ : Exception => BadRequest("Bad Request for input")
  		}  		   
	}
 
  	private def uploadRequestArgs(request : Request[AnyContent])(func : MultipartFormData[TemporaryFile] => JsValue) : Result = {
  		try {
   			request.body.asMultipartFormData.map { x => 
   				Ok(func(x))
  			}.getOrElse (BadRequest("Bad Request for input")) 			  
  		} catch {
  			case _ : Exception => BadRequest("Bad Request for input")
  		}
  	}
  	
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}
	
	def postContent = Action (request => this.requestArgs(request)(PostModule.postContent))
	def uploadFile = Action (request => this.uploadRequestArgs(request)(PostModule.uploadFile))
	def downloadFile(name : String) = Action ( Ok(PostModule.downloadFile(name)).as("image/png"))
}