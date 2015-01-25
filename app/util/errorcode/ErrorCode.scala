package util.errorcode

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue

object ErrorCode {
  	case class ErrorNode(name : String, code : Int, message : String)

  	private def xls : List[ErrorNode] = List(
  		new ErrorNode("token exprie", -1, "inputing token is exprition"),
  		new ErrorNode("token not valid", -2, "inputing token is not valid"),
  		new ErrorNode("wrong validation code", -3, "inputing validation code is not valid or not match to this phone number"),
  		new ErrorNode("phone number not valid", -4, "inputing phone code is not valid"),
  		new ErrorNode("auth token not valid", -5, "the auth token is not validated"),
  		new ErrorNode("post image error", -6, "post image with errors"),
  		new ErrorNode("unknown user", -7, "user is not existing"),
  		new ErrorNode("post token not vaild", -8, "post id or post token not existing"),
  		new ErrorNode("user not existing", -9, "user not exist or delected user")
  	)
  
  	def getErrorCodeByName(name : String) : Int = (xls.find(x => x.name == name)) match {
  			case Some(y) => y.code
  			case None => -9999
  		}
  	
   	def getErrorMessageByName(name : String) : String = (xls.find(x => x.name == name)) match {
  			case Some(y) => y.message
  			case None => "unknow error"
  		}
   	
   	def errorToJson(name : String) : JsValue =
  		Json.toJson(Map("status" -> toJson("error"), "error" -> 
  				toJson(Map("code" -> toJson(this.getErrorCodeByName(name)), "message" -> toJson(this.getErrorMessageByName(name))))))
}