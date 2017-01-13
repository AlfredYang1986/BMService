package module.webpay

import module.sercurity.Sercurity

object WechatSercurityFunc {
    val seed = "alfred yang wechat nonce str generater"
    
    val app_id = "wx66d179d99c9ba7d6"
    val app_secret = "469c1beed3ecaa3a836767a5999beeb1"
    
    val mch_id = "1384946602"
    val mch_key = "dongdaservicedongdaservice123456"
   
    val trade_type_app = "APP"
    val trade_type_web = "JSAPI"
    
    val spbill_create_ip = "127.0.0.1"

    val fake_notify_url = "http://wxpay.weixin.qq.com/pub_v2/pay/notify.php"
    
    def randomNonceString : String = Sercurity.md5Hash((10000.0 * Math.random()).toInt.toString + seed + Sercurity.getTimeSpanWithMillSeconds) 
}