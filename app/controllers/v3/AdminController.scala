package controllers.v3

import play.api._
import play.api.mvc._
import util.dao.from
import module.profile.v2.ProfileModule
import module.kidnap.v3.kidnapModule
import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson
import controllers.common.requestArgsQuery._
import module.timemanager.v3.TMModule

/**
  * Created by Alfred on 13/04/2017.
  */
object AdminController extends Controller {
	def lstServices(p : String) = Action {
		Ok("lst services")
	}

	def lstShopProfile(p : String) = Action {
		try {
			Ok(views.html.lstShops(ProfileModule.queryMultipleProfiles(toJson(Map("skip" -> toJson(p.toInt * 50))))))
		} catch {
			case _ : Exception => BadRequest("wrong args")
		}
	}

	def lstUserServices(u : String) = Action {
		try {
			val result = kidnapModule.mineKidnapService(toJson(Map("owner_id" -> u)))._1.get.get("result").get.asOpt[List[JsValue]].get
			Ok(views.html.lstUserServices(result)(u))

		} catch {
			case _ : Exception => BadRequest("wrong args")
		}
	}

	def detailService(service_id : String) = Action {
		try {
			val result = toJson(kidnapModule.queryKidnapServiceDetail(toJson(Map("service_id" -> service_id)))._1.get)
			Ok(views.html.detailService(result))
		} catch {
			case _ : Exception => BadRequest("wrong args")
		}
	}

	def detailShopProfile(user_id : String) = Action {
		try {
			val shop = toJson(ProfileModule.queryUserProfile(toJson(Map("user_id" -> "", "auth_token" -> "", "owner_user_id" -> user_id)))._1.get)
			Ok(views.html.detailShop(shop))
		} catch {
			case _ : Exception => BadRequest("wrong args")
		}
	}

	def updateShopProfile = Action { request =>
		import pattern.ResultMessage.common_result
		val tf : JsValue => JsValue = js => toJson(common_result((ProfileModule.updateUserProfile(js)(None)._1.get))._1.get)
		requestArgs(request)(tf)
	}

	def updateService = Action { request =>
		import pattern.ResultMessage.common_result
		val tf : JsValue => JsValue = js => {
			val (_, origin) = kidnapModule.queryServiceStatus((js \ "service_id").asOpt[String])
			toJson(common_result((kidnapModule.updateKidnapServiceImpl(js, origin)._1.get))._1.get)
		}
		requestArgs(request)(tf)
	}

	def tmService(service_id : String) = Action { request =>
		try {
			val result = toJson(kidnapModule.queryKidnapServiceDetail(toJson(Map("service_id" -> service_id)))._1.get)
			val tmp = TMModule.queryServiceTM(toJson(Map("service_id" -> service_id)))(None)._1.get
			val tms = tmp.get("tms").map (x => x.asOpt[List[JsValue]].get).getOrElse(Nil)
			Ok(views.html.tmService(result)(tms))
		} catch {
			case _ : Exception => BadRequest("wrong args")
		}
	}

	def updateTMService = Action { request =>
		import pattern.ResultMessage.common_result
		val tf : JsValue => JsValue = js => toJson(common_result(TMModule.updateServiceTM(js)(None)._1.get)._1.get)
		requestArgs(request)(tf)
	}

	def serviceImgsManagement(service_id : String) = Action {
		try {
			val result = toJson(kidnapModule.queryKidnapServiceDetail(toJson(Map("service_id" -> service_id)))._1.get)
			Ok(views.html.serviceImgMag(result))
		} catch {
			case _ : Exception => BadRequest("wrong args")
		}
	}

	def deleteImages = Action (request => requestArgs(request)(kidnapModule.deleteImage))

	def shopImgsManagement(user_id : String) = Action {
		try {
			val shop = toJson(ProfileModule.queryUserProfile(toJson(Map("user_id" -> "", "auth_token" -> "", "owner_user_id" -> user_id)))._1.get)
			Ok(views.html.shopImageManagement(shop))
		} catch {
			case _ : Exception => BadRequest("wrong args")
		}
	}

	def deleteShopImages = Action (request => requestArgs(request)(ProfileModule.deleteShopImage))
}
