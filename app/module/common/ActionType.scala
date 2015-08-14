package module.common

object AcitionType {
  case object follow extends ActionTypeDefines(0)
  case object unfollow extends ActionTypeDefines(1)
  case object like extends ActionTypeDefines(2)
  case object push extends ActionTypeDefines(3)
  case object message extends ActionTypeDefines(4) // may not use
}

sealed abstract class ActionTypeDefines(val index : Int)