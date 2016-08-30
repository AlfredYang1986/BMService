package module.webpay

import module.sercurity.Sercurity

object WechatSercurityFunc {
    val seed = "alfred yang wechat nonce str generater"
    
    val app_id = ""
    val app_secret = ""
    
    val mch_id = ""
    val mch_key = ""
   
    val trade_type_app = "APP"
    val trade_type_web = "JSAPI"
    
    val spbill_create_ip = "127.0.0.1"

    val fake_notify_url = "http://wxpay.weixin.qq.com/pub_v2/pay/notify.php"
    
    def randomNonceString : String = Sercurity.md5Hash((10000.0 * Math.random()).toInt.toString + seed + Sercurity.getTimeSpanWithMillSeconds) 
}