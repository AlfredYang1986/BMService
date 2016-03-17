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
}

sealed abstract class ActionTypeDefines(val index : Int)