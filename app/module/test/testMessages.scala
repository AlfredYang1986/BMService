package module.test

import play.api.libs.json.JsValue
import dongdamessages.CommonMessage

abstract class msg_TestCommand extends CommonMessage

object testMessages {
	case class msg_Test_1(data : JsValue) extends msg_TestCommand
	case class msg_Test_2(data : JsValue) extends msg_TestCommand
	case class msg_Test_3(data : JsValue) extends msg_TestCommand
}