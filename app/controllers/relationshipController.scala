package controllers

import play.api.mvc._

import module.relationship.RelationshipModule

import controllers.common.requestArgsQuery.{requestArgs, requestArgsWithAuthCheck}

object relationshipController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def follow = Action (request => requestArgsWithAuthCheck(request)(RelationshipModule.follow))
	def unfollow = Action (request => requestArgsWithAuthCheck(request)(RelationshipModule.unfollow))
	def queryFollowingUsers = Action (request => requestArgsWithAuthCheck(request)(RelationshipModule.queryFollowingUsers))
	def queryFollowedUsers = Action (request => requestArgsWithAuthCheck(request)(RelationshipModule.queryFollowedUsers))
	def queryMutureFollowingUsers = Action (request => requestArgs(request)(RelationshipModule.queryMutureFollowingUsers))
	def queryRelationsBetweenUsers = Action (request => requestArgs(request)(RelationshipModule.queryRelationsBetweenUsers))
	
	def askPhoneAddressBookFriendsJoin = Action (request => requestArgsWithAuthCheck(request)(RelationshipModule.askPhoneAddressBookFriendsJoin))
}