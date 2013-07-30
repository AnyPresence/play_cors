import com.anypresence.playframework.cors.FakeController

import play.api.mvc.Handler
import play.api.mvc.RequestHeader

object Routes extends play.core.Router.Routes {
  
  private var thePrefix: String = null
  
  def documentation: Seq[(String, String, String)] = Seq()
  def setPrefix(prefix: String) = thePrefix = prefix
  def prefix: String = thePrefix
  
  def routes: PartialFunction[RequestHeader, Handler] = {
    case req if (req.method == "GET" && req.path == "/index.html") => FakeController.index
    case req if (Seq("GET", "POST", "PUT", "DELETE").contains(req.method) && req.path == "/file/at/1.html") => FakeController.index
    case req if (Seq("GET", "POST", "HEAD", "DELETE").contains(req.method) && req.path == "/file/list_all") => FakeController.index
    case req if (Seq("GET", "POST", "HEAD", "DELETE").contains(req.method) && req.path == "/file/list_none") => FakeController.index    
    case req if (req.method == "GET" && req.path == "/file/doesnt_support_creds") => FakeController.index         
    case req if (req.method == "GET" && req.path == "/public/cats") => FakeController.index

    case req if (req.method =="OPTIONS") => FakeController.fakeRouteForOptions
  }
  
}
