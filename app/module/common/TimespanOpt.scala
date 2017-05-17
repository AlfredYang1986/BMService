package module.common

import java.util.{Calendar, Date}

/**
  * Created by alfredyang on 16/05/2017.
  */
object TimespanOpt {
    def todayRange : (Long, Long) = {
        val cal = Calendar.getInstance()
        cal.setTime(new Date())
        cal.set(Calendar.HOUR_OF_DAY, 0)
        cal.set(Calendar.MINUTE, 0)
        cal.set(Calendar.SECOND, 0)

        val s = cal.getTime.getTime
        println(s"today date time ${s}")
        cal.add(Calendar.DAY_OF_YEAR, 1)
        val e = cal.getTime.getTime
        println(s"tommorrw date time ${e}")
        (s, e)
    }
}
