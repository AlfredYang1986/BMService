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
  		new ErrorNode("user not existing", -9, "user not exist or delected user"),
  		new ErrorNode("user have low authrity", -10, "can not complete the operation due to low authrity level"),
  		new ErrorNode("group is not exist", -11, "parent for this sub group is no longer exist"),
  		new ErrorNode("already friends", -12, "these two people are already friends"),
  		new ErrorNode("create chat group error", -13, "create chat group error"),
  		new ErrorNode("dismiss chat group error", -14, "dismiss chat group error"),
  		new ErrorNode("not allowed", -15, "operation not allowed"),
  		new ErrorNode("service not existing", -16, "service not exist"),
  		new ErrorNode("wechat pay error", -17, "wechat pay error"),

		new ErrorNode("wrong input", -18, "arguments error for server invoke"),
  		new ErrorNode("em register error", -19, "em register error"),
  		new ErrorNode("can not parse result", -20, "can not parse result"),
		new ErrorNode("log struct error", -21, "log struct error"),
  		
  		new ErrorNode("unknown error", -999, "unknown error")
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