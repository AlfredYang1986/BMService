package controllers

import play.api.mvc._
import module.post.PostModule

import controllers.common.requestArgsQuery._

object postController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}
	
	def postContent = Action (request => requestArgs(request)(PostModule.postContent))
	def uploadFile = Action (request => uploadRequestArgs(request)(PostModule.uploadFile))
	def postComment = Action (request => requestArgs(request)(PostModule.postCommnet))
}