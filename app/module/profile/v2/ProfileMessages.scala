package module.profile.v2

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

import util.errorcode.ErrorCode

abstract class msg_ProfileCommand extends CommonMessage

object ProfileMessages {
	case class msg_CreateProfile(data : JsValue) extends msg_ProfileCommand
	case class msg_UpdateProfile(data : JsValue) extends msg_ProfileCommand
	case class msg_UpdateProfileWithoutResult(data : JsValue) extends msg_ProfileCommand
	case class msg_QueryProfile(data : JsValue) extends msg_ProfileCommand
	case class msg_ChangeToServiceProvider(data : JsValue) extends msg_ProfileCommand
	
	case class msg_OwnerLstNamePhoto(data : JsValue) extends msg_ProfileCommand
	case class msg_UserLstNamePhoto(data : JsValue) extends msg_ProfileCommand
	case class msg_OneOwnerNamePhoto(data : JsValue) extends msg_ProfileCommand
}