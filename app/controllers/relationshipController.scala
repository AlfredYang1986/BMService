package controllers

import play.api.mvc._

import module.relationship.RelationshipModule

import controllers.common.requestArgsQuery.{requestArgs}

object relationshipController extends Controller {
	def index = Action {
		Ok(views.html.index("Your new application is ready."))
	}

	def follow = Action (request => requestArgs(request)(RelationshipModule.follow))
	def unfollow = Action (request => requestArgs(request)(RelationshipModule.unfollow))
	def queryFollowingUsers = Action (request => requestArgs(request)(RelationshipModule.queryFollowingUsers))
	def queryFollowedUsers = Action (request => requestArgs(request)(RelationshipModule.queryFollowedUsers))
	def queryMutureFollowingUsers = Action (request => requestArgs(request)(RelationshipModule.queryMutureFollowingUsers))
	def queryRelationsBetweenUsers = Action (request => requestArgs(request)(RelationshipModule.queryRelationsBetweenUsers))
	
	def askPhoneAddressBookFriendsJoin = Action (request => requestArgs(request)(RelationshipModule.askPhoneAddressBookFriendsJoin))
}