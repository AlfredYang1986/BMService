package module.webpay

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
//import module.common.helpOptions
import java.util.Date

import module.common.http.HTTP

object WechatPayModule {
    def prepayid(data : JsValue, order_id : String) : JsValue = 
        try {
//            val pay_body = (data \ "pay_body").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val pay_body = (data \ "pay_body").asOpt[String].map (x => x).getOrElse("咚哒服务费")
            val pay_noncestr = WechatSercurityFunc.randomNonceString
           
//            val trade_no = (data \ "order_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val trade_no = if (order_id.isEmpty) throw new Exception("wrong input")
                           else order_id
            
           val fee = (data \ "total_fee").asOpt[Int].map (x => x).getOrElse(throw new Exception("wrong input"))
            val spbill_create_ip = (data \ "spbill_create_ip").asOpt[String].map (x => x).getOrElse(WechatSercurityFunc.spbill_create_ip)
           
        		val str_pay = "appid=" + WechatSercurityFunc.app_id + 
        		              "&body=" + pay_body + 
        		              "&mch_id=" + WechatSercurityFunc.mch_id + 
        		              "&nonce_str=" + pay_noncestr + 
        		              "&notify_url="+ WechatSercurityFunc.fake_notify_url + 
        		              "&out_trade_no=" + trade_no + 
        		              "&spbill_create_ip=" + WechatSercurityFunc.spbill_create_ip + 
        		              "&total_fee=" + fee + 
        		              "&trade_type=" + WechatSercurityFunc.trade_type_app + 
        		              "&key=" + WechatSercurityFunc.mch_key
        	
        		val str_md5 = module.sercurity.Sercurity.md5Hash(str_pay).toUpperCase
        		val valxml = """<xml>
        		                  <appid>%s</appid>
        		                  <body><![CDATA[%s]]></body>
        		                  <mch_id>%s</mch_id>
        		                  <nonce_str>%s</nonce_str>
        		                  <notify_url>%s</notify_url>
        		                  <out_trade_no>%s</out_trade_no>
        		                  <spbill_create_ip>127.0.0.1</spbill_create_ip>
        		                  <total_fee>%d</total_fee>
        		                  <trade_type>%s</trade_type>
        		                  <sign><![CDATA[%s]]></sign>
        		                </xml>"""
        		  			.format(WechatSercurityFunc.app_id, pay_body, WechatSercurityFunc.mch_id, pay_noncestr, WechatSercurityFunc.fake_notify_url, trade_no, fee, WechatSercurityFunc.trade_type_app, str_md5)
        	
        		val order_url = "https://api.mch.weixin.qq.com/pay/unifiedorder"
        		val result = ((HTTP(order_url)).post(valxml.toString))
        	
        		val tag_return_code = "return_code"
        		var return_code = result.substring(result.indexOf(tag_return_code) + tag_return_code.length + 1, result.indexOf("</" + tag_return_code)) 
        		if (return_code.startsWith("<![CDATA[") && return_code.endsWith("]]>")) 
        			  return_code = return_code.substring(9, return_code.length - 3)
        	
        	  if (return_code.toLowerCase() == "success") {
             		val tag = "prepay_id"
            		var prepay_id = result.substring(result.indexOf(tag) + tag.length + 1, result.indexOf("</" + tag)) 
            		if (prepay_id.startsWith("<![CDATA[") && prepay_id.endsWith("]]>")) 
            			  prepay_id = prepay_id.substring(9, prepay_id.length - 3)
            			  
            		toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("prepay_id" -> prepay_id, "order_id" -> trade_no))))
        	  } else {
        	    throw new Exception("wechat pay error")
        	  }
        } catch {
          case ex : Exception => { println(ex.getStackTrace); ErrorCode.errorToJson(ex.getMessage) }
        }
}