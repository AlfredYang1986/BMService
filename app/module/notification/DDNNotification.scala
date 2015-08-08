package module.notification

import module.common.http._
import play.api.libs.json.Json.{toJson}

object DDNNotification {
	
	val XMPP = "https://rest.gotye.com.cn/api/"
  
	def getAuthTokenForXMPP = {
		HTTP(XMPP + "accessToken").header("Accept" -> "application/json", "Content-Type" -> "application/json").
			post("username" -> toJson("358669625@qq.com"), "grant_type" -> toJson("password"), "password" -> toJson("Abcde196125"))
	}
}