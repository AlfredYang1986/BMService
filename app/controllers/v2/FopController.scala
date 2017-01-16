package controllers.v2

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._
import module.common.files.fop

object FopController extends Controller {
    def uploadFile = Action (request => uploadRequestArgs(request)(fop.uploadFile))
    def downloadFile(name : String) = Action ( Ok(fop.downloadFile(name)).as("image/png"))
}
