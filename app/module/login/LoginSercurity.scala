package module.login

import java.security._
import java.util.Date

object LoginSercurity {
	def md5Hash(text: String) : String = java.security.MessageDigest.getInstance("MD5").digest(text.getBytes()).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
	def getTimeSpanWithMillSeconds : String = String.valueOf(new Date().getTime())
	def getTimeSpanWithSeconds : String = String.valueOf(new Date().getTime() / 1000)
	def getTimeSpanWithMinutes : String = String.valueOf(new Date().getTime() / (1000 * 60))
	def getTimeSpanWith10Minutes : String = String.valueOf(new Date().getTime() / (1000 * 60 * 10))
}