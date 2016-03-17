package controllers

import play.api.mvc._
import module.post.PostModule

import controllers.common.requestArgsQuery._

object postController extends Controller {
	def postContent = Action (request => requestArgsWithAuthCheck(request)(PostModule.postContent))
	def uploadFile = Action (request => uploadRequestArgs(request)(PostModule.uploadFile))
	def postComment = Action (request => requestArgs(request)(PostModule.postCommnet))
	def postLike = Action (request => requestArgsWithAuthCheck(request)(PostModule.postLike))
	def postPush = Action (request => requestArgsWithAuthCheck(request)(PostModule.postPush))
	
	def postUnlike = Action (request => requestArgsWithAuthCheck(request)(PostModule.postUnlike))
	def postUnpush = Action (request => requestArgsWithAuthCheck(request)(PostModule.postUnpush))
}