package module.kidnap.v3

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

import util.errorcode.ErrorCode

abstract class msg_KidnapServiceCollectionCommand extends CommonMessage

object kidnapCollectionMessages {
	case class msg_CollectionService(data : JsValue) extends msg_KidnapServiceCollectionCommand
	case class msg_UncollectionService(data : JsValue) extends msg_KidnapServiceCollectionCommand
	case class msg_UserCollectionLst(data : JsValue) extends msg_KidnapServiceCollectionCommand

	case class msg_IsUserCollectLst(data : JsValue) extends msg_KidnapServiceCollectionCommand
	case class msg_IsUserCollect(data : JsValue) extends msg_KidnapServiceCollectionCommand
}