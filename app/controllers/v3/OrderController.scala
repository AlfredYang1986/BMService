package controllers.v3

import controllers.common.requestArgsQuery._
import dongdamessages.MessageRoutes
import module.kidnap.v2.kidnapCollectionMessages.msg_IsUserCollectLst
import module.kidnap.v3.kidnapMessages.msg_ServiceForOrders
import module.order.v3.orderMessages._
import module.profile.v2.ProfileMessages.{msg_OwnerLstNamePhoto, msg_UserLstNamePhoto}
import pattern.LogMessage.msg_log
import pattern.ParallelMessage
import pattern.ResultMessage.{msg_CommonResultMessage, msg_PlusDateToResultMessage}
import play.api.libs.json.Json.toJson
import play.api.mvc._

object OrderController extends Controller {
    def postOrder = Action (request => requestArgsV2(request) { jv =>
            import pattern.LogMessage.common_log
            import pattern.ResultMessage.common_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("v3 post order"))), jv)
                :: msg_PostOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
        })

    def rejectOrder = Action (request => requestArgsV2(request) { jv =>
            import pattern.LogMessage.common_log
            import pattern.ResultMessage.common_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("v3 reject order"))), jv)
                :: msg_CheckOwner(jv) :: msg_Reject(jv) :: msg_CommonResultMessage() :: Nil, None)
        })

    def acceptOrder = Action (request => requestArgsV2(request) { jv =>
            import pattern.LogMessage.common_log
            import pattern.ResultMessage.common_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("v3 accept order"))), jv)
                :: msg_CheckOwner(jv) :: msg_Accept(jv) :: msg_CommonResultMessage() :: Nil, None)
        })

    def cancelOrder = Action (request => requestArgsV2(request) { jv =>
            import pattern.LogMessage.common_log
            import pattern.ResultMessage.common_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("v3 cancel order"))), jv)
                :: msg_CheckUser(jv) :: msg_Cancel(jv) :: msg_CommonResultMessage() :: Nil, None)
        })

    def accomplishOrder = Action (request => requestArgsV2(request) { jv =>
            import pattern.LogMessage.common_log
            import pattern.ResultMessage.common_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("v3 accomplish order"))), jv)
                :: msg_accomplishOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
        })

    def prepayOrder = Action (request => requestArgsV2(request) { jv =>
            import pattern.LogMessage.common_log
            import pattern.ResultMessage.common_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("v3 prepay order"))), jv)
                :: msg_CheckUser(jv) :: msg_Prepay(jv) :: msg_CommonResultMessage() :: Nil, None)
        })

    def postpayOrder = Action (request => requestArgsV2(request) { jv =>
            import pattern.LogMessage.common_log
            import pattern.ResultMessage.common_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("v3 post pay order"))), jv)
                :: msg_CheckUser(jv) :: msg_Postpay(jv) :: msg_CommonResultMessage() :: Nil, None)
        })

//	def pushOrder = Action (request => requestArgsV2(request) { jv =>
//			import pattern.LogMessage.common_log
//			import pattern.ResultMessage.common_result
//    		MessageRoutes(msg_log(toJson(Map("method" -> toJson("push order"))), jv)
//			    :: msg_PushOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
//		})
//	def pushOrderAlipay = Action (request => requestArgsV2(request) { jv =>
//		import pattern.LogMessage.common_log
//		import pattern.ResultMessage.common_result
//		MessageRoutes(msg_log(toJson(Map("method" -> toJson("push order Alipay"))), jv)
//			:: msg_PushOrderAlipay(jv) :: msg_CommonResultMessage() :: Nil, None)
//	})
	def popOrder = Action (request => requestArgsV2(request) { jv =>
			import pattern.LogMessage.common_log
			import pattern.ResultMessage.common_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("pop order"))), jv)
			    :: msg_popOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
	def queryOrders = Action (request => requestArgsV2(request) { jv =>
			import pattern.LogMessage.common_log
			import pattern.ResultMessage.lst_result
            MessageRoutes(msg_log(toJson(Map("method" -> toJson("query orders"))), jv)
			    :: msg_queryOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
		})
//	def acceptOrder = Action (request => requestArgsV2(request) { jv =>
//			import pattern.LogMessage.common_log
//			import pattern.ResultMessage.common_result
//            MessageRoutes(msg_log(toJson(Map("method" -> toJson("accept order"))), jv)
//			    :: msg_acceptOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
//		})
//	def rejectOrder = Action (request => requestArgsV2(request) { jv =>
//			import pattern.LogMessage.common_log
//			import pattern.ResultMessage.common_result
//            MessageRoutes(msg_log(toJson(Map("method" -> toJson("reject order"))), jv)
//			    :: msg_rejectOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
//		})
//	def accomplishOrder = Action (request => requestArgsV2(request) { jv =>
//			import pattern.LogMessage.common_log
//			import pattern.ResultMessage.common_result
//            MessageRoutes(msg_log(toJson(Map("method" -> toJson("accomplish order"))), jv)
//                :: msg_accomplishOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
//		})
	def updateOrder = Action (request => requestArgsV2(request) { jv =>
        import pattern.LogMessage.common_log
        import pattern.ResultMessage.common_result
        MessageRoutes(msg_log(toJson(Map("method" -> toJson("update order"))), jv)
            :: msg_updateOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
    })
//    def payOrder = Action (request => requestArgsV2(request) { jv =>
//        import pattern.LogMessage.common_log
//        import pattern.ResultMessage.common_result
//        MessageRoutes(msg_log(toJson(Map("method" -> toJson("pay order"))), jv)
//            :: msg_payOrder(jv) :: msg_CommonResultMessage() :: Nil, None)
//    })

    def queryApplyOrders = queryOrders
	def queryOwnerOrders = queryOrders
	def queryApplyOrders2 = queryOrders2
	def queryOwnerOrders2 = queryOrders2
		
	def queryOrders2 = Action (request => requestArgsV2(request) { jv => 
		import module.order.v3.orderModule.{orderFinalMerge, orderOrderMerge, orderResultMerge}
		import pattern.LogMessage.common_log
		import pattern.ResultMessage.lst_result

		val service_sub = ParallelMessage(
							MessageRoutes(msg_ServiceForOrders(jv) :: Nil, None) ::
							MessageRoutes(msg_OwnerLstNamePhoto(jv) :: Nil, None) ::
							MessageRoutes(msg_IsUserCollectLst(jv) :: Nil, None) :: Nil, orderResultMerge)

		val order_sub = ParallelMessage(
							MessageRoutes(msg_UserLstNamePhoto(jv) :: Nil, None) ::
							Nil, orderOrderMerge)

		val para = ParallelMessage(
						MessageRoutes(service_sub :: Nil, None) ::
						MessageRoutes(order_sub :: Nil, None) :: Nil, orderFinalMerge)

		MessageRoutes(msg_log(toJson(Map("method" -> toJson("query orders"))), jv)
			:: msg_queryOrder(jv) :: para :: msg_CommonResultMessage()
            :: msg_PlusDateToResultMessage(jv) :: Nil, None)
	})

	def splitOrderTimelst = Action (request => requestArgsV2(request) { jv =>
        import module.order.v3.orderModule.{orderFinalMerge, orderOrderMerge, orderResultMerge}
        import pattern.LogMessage.common_log
        import pattern.ResultMessage.lst_result

        val service_sub = ParallelMessage(
            MessageRoutes(msg_ServiceForOrders(jv) :: Nil, None) ::
                MessageRoutes(msg_OwnerLstNamePhoto(jv) :: Nil, None) ::
                MessageRoutes(msg_IsUserCollectLst(jv) :: Nil, None) :: Nil, orderResultMerge)

        val order_sub = ParallelMessage(
            MessageRoutes(msg_UserLstNamePhoto(jv) :: Nil, None) ::
                Nil, orderOrderMerge)

        val para = ParallelMessage(
            MessageRoutes(service_sub :: Nil, None) ::
                MessageRoutes(order_sub :: Nil, None) :: Nil, orderFinalMerge)

        MessageRoutes(msg_log(toJson(Map("method" -> toJson("query orders"))), jv)
            :: msg_queryOrder(jv) :: para :: msg_splitOrderTimes(jv) :: msg_CommonResultMessage()
            :: msg_PlusDateToResultMessage(jv) :: Nil, None)
    })
}