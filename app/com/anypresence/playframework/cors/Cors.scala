package com.anypresence.playframework.cors

import play.api.GlobalSettings
import play.api.Play.current
import play.api.Logger._
import play.api.http.HeaderNames._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.AnyContent
import play.api.mvc.AsyncResult
import play.api.mvc.Filter
import play.api.mvc.Handler
import play.api.mvc.PlainResult
import play.api.mvc.RequestHeader
import play.api.mvc.Request
import play.api.mvc.WrappedRequest
import play.api.mvc.Result
import play.api.mvc.Results._

object Cors {
  
  lazy val config: Seq[CorsConfig] = CorsConfigReader.parseConfig
  
  lazy val origins: Map[String, Seq[Resource]] = 
    config.map { corsConfig: CorsConfig =>
      corsConfig.origins.map { (_ -> corsConfig.resources) }
    }.flatten.toMap
  
  val noop: (PlainResult) => Result = { result => result }
  
  val corsFilter = Filter { (next, req) =>
    
    implicit val requestedResource = req.path
    debug("Resource requested is " + requestedResource)
        
    val resultHandlerFunction: (PlainResult) => Result = req.headers.get(ORIGIN).map { implicit origin =>
      debug("Received cors request for origin " + origin)
      
      val resourcesMaybe = if (origins.contains(origin)) { 
        debug("Matched origin " + origin)
        Some(origins(origin))
      } else if (origins.contains("*")) {
        debug("* matched origin " + origin)
        Some(origins("*"))
      } else {
        debug("origin " + origin + " not found")
        None
      }
      
      val funcMaybe = resourcesMaybe.flatMap { resources: Seq[Resource] => 
        debug("Resources " + resources + " found for origin " + origin + " and requested resource " + requestedResource)
        val function = resources.find { resource: Resource => 
          resource.resourcePattern == requestedResource || (resource.resourcePattern.endsWith("*") && requestedResource.startsWith(resource.resourcePattern.dropRight(1)))
        }.map { resource: Resource =>
          if (isSimpleRequest(req)) {
            debug("Simple request detected for resource " + requestedResource)
            handleSimple(origin, requestedResource, resource)
          } else if (isPreflightRequest(req)) { 
            debug("Preflight request detected for resource " + requestedResource)
            handlePreflight(req, resource, origin)
          } else {
            debug("Received request with origin header, but was not a valid simple request or preflight request")
            noop
          }
        }.getOrElse{
          debug("Unable to find matching resource among " + resources + " for origin " + origin + " and requestedResource " + requestedResource)
          null
        }
        Option(function)
      }
      
      funcMaybe.getOrElse(noop)
    }.getOrElse {
      debug("Received Non-CORS request")
      noop
    }
    
    if (isPreflightRequest(req)) {
      val result = current.routes.map { routes => 
        val wrapped = new WrappedRequest[AnyContent](req.asInstanceOf[Request[AnyContent]]) {
          override def method = headers(ACCESS_CONTROL_REQUEST_METHOD)
        }
        if (routes.handlerFor(wrapped).isDefined) {
          debug("Preflight request returning Ok because route is defined")
          Ok
        } else {
          NotFound
        }
      }.getOrElse(NotFound)
      resultHandlerFunction(result)
    } else {
      next(req) match {
        case plain: PlainResult => resultHandlerFunction(plain)
        case async: AsyncResult => async.transform(resultHandlerFunction)
      }    
    }
    
  }
  
  private def handlePreflight(request: RequestHeader, resource: Resource, origin: String): (PlainResult) => Result = {
    debug("Handling preflight request")
    
    request.headers.get(ACCESS_CONTROL_REQUEST_METHOD).map { method:String =>
      
      debug("" + ACCESS_CONTROL_REQUEST_METHOD + " is " + method)
      val accessControlRequestHeaders = request.headers.get(ACCESS_CONTROL_REQUEST_HEADERS).map { accessControlRequestHeaders: String => 
        accessControlRequestHeaders.split(",").map { _.trim() }
      }.getOrElse(Array())
      
      debug("After parsing " + ACCESS_CONTROL_REQUEST_HEADERS + ", value is " + accessControlRequestHeaders)
      
      if (resource.methods.contains(method)) { 
        
        val unexpectedHeader = accessControlRequestHeaders.find { accessControlRequestHeader: String =>
          !resource.headers.find(_ == accessControlRequestHeader.toUpperCase()).isDefined
        }
        
        if (unexpectedHeader.isDefined) { 
          debug("Header " + unexpectedHeader.get + " was not in the list of expected headers allowed by the resource.  Allowed headers are " + resource.headers)
          noop
        } else {
          (result: PlainResult) => { 
            val originVal = if (resource.supportsCredentials) origin else "*"
            var newResult = result.withHeaders(ACCESS_CONTROL_ALLOW_ORIGIN -> originVal)
            newResult = resource.maxAge.map { maxAge => newResult.withHeaders(ACCESS_CONTROL_MAX_AGE -> maxAge.toString()) }.getOrElse(newResult)
            newResult.withHeaders(ACCESS_CONTROL_ALLOW_METHODS -> resource.methods.mkString(", "))
            newResult.withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> resource.headers.mkString(", "))
            newResult
          }
        }
      } else {
        debug("Method " + method + " not contained in allowed methods " + resource.methods + " for resource " + resource)
        noop
      }
    }.getOrElse {
      debug("Header " + ACCESS_CONTROL_REQUEST_METHOD + " missing")
      noop
    }
  }
  
  private def handleSimple(origin: String, requestedResource: String, resource: Resource): (PlainResult) => Result = {
    debug("Resource " + resource + " found for origin " + origin + " and requested resource " + requestedResource)
    val originValue = if (resource.supportsCredentials) origin else "*"
    debug("Responding with ACCESS_CONTROL_ALLOW_ORIGIN " + originValue + " because resources.supportsCredentials is " + resource.supportsCredentials)
    debug("Headers to espose are " + resource.expose)
    (result: PlainResult) => {
      result.withHeaders(ACCESS_CONTROL_ALLOW_ORIGIN -> originValue, ACCESS_CONTROL_EXPOSE_HEADERS -> resource.expose.mkString(","))
    }
  }
  
  private def isSimpleRequest(implicit request: RequestHeader): Boolean = isSimpleRequestMethod && containsOnlySimpleHeaders && containsSimpleContentType
  
  private def isSimpleRequestMethod(implicit request: RequestHeader): Boolean = Seq("GET", "HEAD", "POST").contains(request.method)
  
  private def containsOnlySimpleHeaders(implicit request: RequestHeader): Boolean = {
    !request.headers.keys.exists { !Seq(ACCEPT, ACCEPT_LANGUAGE, CONTENT_LANGUAGE, CONTENT_TYPE).contains(_) }
  }
  
  private def containsSimpleContentType(implicit request: RequestHeader): Boolean = {
    request.headers.get(CONTENT_TYPE).map { contentType: String =>
      Seq("application/x-www-form-urlencoded", "multipart/form-data", "text/plain").contains(contentType)
    }.getOrElse(false)
  }
  
  private def isPreflightRequest(implicit request: RequestHeader): Boolean = request.method == "OPTIONS" && request.headers.get(ORIGIN).isDefined
  
}
