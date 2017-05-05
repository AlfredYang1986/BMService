package module.common

object AcitionType {
  case object follow extends ActionTypeDefines(0)
  case object unfollow extends ActionTypeDefines(1)
  case object like extends ActionTypeDefines(2)
  case object push extends ActionTypeDefines(3)
  case object loginOtherDevice extends ActionTypeDefines(4)
  case object unlike extends ActionTypeDefines(5)
  case object unpush extends ActionTypeDefines(6)
  case object message extends ActionTypeDefines(7) // may not use
  
  /**
   * order notify
   */
  case object orderPushed extends ActionTypeDefines(10)
  case object orderAccecpted extends ActionTypeDefines(11)
  case object orderRejected extends ActionTypeDefines(12)
  case object orderAccomplished extends ActionTypeDefines(13)

  /**
   * order notify v3
   */
  case object order_v3_posted extends ActionTypeDefines(20)
  case object order_v3_rejected extends ActionTypeDefines(21)
  case object order_v3_accepted extends ActionTypeDefines(22)
  case object order_v3_paid extends ActionTypeDefines(23)
  case object order_v3_cancel extends ActionTypeDefines(24)
  case object order_v3_done extends ActionTypeDefines(25)
}

sealed abstract class ActionTypeDefines(val index : Int)