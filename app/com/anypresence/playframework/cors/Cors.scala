package com.anypresence.playframework.cors

import com.typesafe.config.ConfigObject

import play.api.Configuration
import play.api.GlobalSettings
import play.api.Logger._
import play.api.Play.current
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

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

trait CorsImpl {
  
  private lazy val config: Seq[CorsConfig] = CorsConfigReader.parseConfig
  
  private lazy val origins: Map[String, Seq[Resource]] = 
    config.map { corsConfig: CorsConfig =>
      corsConfig.origins.map { (_ -> corsConfig.resources) }
    }.flatten.toMap
  
  private val noop: (PlainResult) => Result = { result => result }
  
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
          debug("Requested resource is " + requestedResource + " .... compared with " + resource)
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
          debug("Preflight request returning NotFound because route is not defined")          
          NotFound
        }
      }.getOrElse {
        debug("No routes found for current application")
        NotFound
      }
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
        
        var allowAllHeaders = false
        val resourceHeadersUpperCase = resource.headers.map { header:String => 
          if (header == "*" || header.toUpperCase() == "ANY") allowAllHeaders = true
          header.toUpperCase() 
        }
        
        val unexpectedHeader = accessControlRequestHeaders.find { accessControlRequestHeader: String =>
          !resourceHeadersUpperCase.find(_ == accessControlRequestHeader.toUpperCase()).isDefined
        }
        
        if (unexpectedHeader.isDefined) { 
          debug("Header " + unexpectedHeader.get + " was not in the list of expected headers allowed by the resource.  Allowed headers are " + resource.headers)
          noop
        } else {
          (result: PlainResult) => { 
            val permittedHeaders = if (allowAllHeaders) request.headers.keys() else resource.headers
            
            val originVal = if (resource.supportsCredentials) origin else "*"
            var newResult = result.withHeaders(ACCESS_CONTROL_ALLOW_ORIGIN -> originVal)
            newResult = resource.maxAge.map { maxAge => newResult.withHeaders(ACCESS_CONTROL_MAX_AGE -> maxAge.toString()) }.getOrElse(newResult)
            newResult = newResult.withHeaders(ACCESS_CONTROL_ALLOW_METHODS -> resource.methods.mkString(", "))
            newResult = newResult.withHeaders(ACCESS_CONTROL_ALLOW_HEADERS -> permittedHeaders.mkString(", "))
            newResult = if (resource.supportsCredentials) newResult.withHeaders(ACCESS_CONTROL_ALLOW_CREDENTIALS -> "true") else newResult
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
    debug("Headers to expose are " + resource.expose)
    (result: PlainResult) => {
      var r = result.withHeaders(ACCESS_CONTROL_ALLOW_ORIGIN -> originValue)
      r = if (resource.supportsCredentials) r.withHeaders(ACCESS_CONTROL_ALLOW_CREDENTIALS -> "true") else r
      r = if (!resource.expose.isEmpty) r.withHeaders(ACCESS_CONTROL_EXPOSE_HEADERS -> resource.expose.mkString(",")) else r
      r
    }
  }
  
  private def isSimpleRequest(implicit request: RequestHeader): Boolean = isSimpleRequestMethod && containsOnlySimpleHeaders && containsSimpleContentType
  
  private def isSimpleRequestMethod(implicit request: RequestHeader): Boolean = {
    debug("request method : " + request.method)
    Seq("GET", "HEAD", "POST").contains(request.method)
  }
  
  private def containsOnlySimpleHeaders(implicit request: RequestHeader): Boolean = {
    debug("request headers : " + request.headers.keys)
    !request.headers.keys.exists { x:String => !Seq(ACCEPT.toUpperCase(), ACCEPT_LANGUAGE.toUpperCase(), CONTENT_LANGUAGE.toUpperCase(), CONTENT_TYPE.toUpperCase(), ORIGIN.toUpperCase()).contains(x.toUpperCase()) }
  }
  
  private def containsSimpleContentType(implicit request: RequestHeader): Boolean = {
    debug("content type: " + request.headers.get(CONTENT_TYPE))
    request.headers.get(CONTENT_TYPE).map { contentType: String =>
      Seq("application/x-www-form-urlencoded", "multipart/form-data", "text/plain").contains(contentType.toLowerCase())
    }.getOrElse(false)
  }

  private def isPreflightRequest(implicit request: RequestHeader): Boolean = request.method == "OPTIONS" && request.headers.get(ORIGIN).isDefined && request.headers.get(ACCESS_CONTROL_REQUEST_METHOD).isDefined
  
}

case class CorsConfig(origins: Seq[String], resources: Seq[Resource])
case class Resource(resourcePattern: String, methods: Seq[String], headers: Seq[String], expose: Seq[String], supportsCredentials: Boolean, maxAge: Option[Long])

protected[cors] object CorsConfigReader {
  
  private val validHttpMethods = Seq[String]("GET", "PUT", "POST", "DELETE", "TRACE", "CONNECT", "OPTIONS", "HEAD")
  
  def parseConfig: Seq[CorsConfig] = {
    val config = current.configuration
    
    debug("Parsing configuration")
    config.getObject("cors").map { cors =>
      cors.getObjectList("allow").map { allowedConfigs =>
        allowedConfigs.zipWithIndex.map { tuple: Tuple2[ConfigObject, Int] =>
          parseCorsConfig(tuple._1, tuple._2)
        }
      }.getOrElse(Seq[CorsConfig]())
    }.getOrElse(throw reportError("cors", "Cors configuration missing from application.conf -- Unable to continue", config))
  }
  
  private def reportError(key: String, message: String, config: Configuration) = {
    info("CORS improperly configured.\n\nExample configuration: " + corsConfigExample)
    config.reportError(key, message)
  }
  
  private def parseCorsConfig(config: Configuration, index: Int): CorsConfig = {
    config.getStringList("origins").map { allowedOrigins =>
      config.getObjectList("resources").map { resources =>
        CorsConfig(allowedOrigins, parseResources(resources, index))
      }.getOrElse(throw reportError("cors.allow.resources", "Must provide one ore more resources in cors.allow[" + index + "].resources", config))
    }.getOrElse(throw reportError("cors.allow.origins", "Must provide one or more origins in cors.allow[" + index + "].origins", config))
  }
  
  private def validateMethods(methods: Seq[String], allowIndex: Int, resourceIndex: Int, config: Configuration) = {
    methods.foreach { method: String =>
      if (!validHttpMethods.contains(method)) {
        throw reportError("cors.allow.resources.methods", "methods in cors.allow[" + allowIndex + "].resources[" + resourceIndex + "].methods must be one of " + validHttpMethods, config)
      }
    }
    methods
  }
  
  private def parseResources(resources: Seq[ConfigObject], allowIndex: Int): Seq[Resource] = {
    resources.zipWithIndex.map { tuple: Tuple2[ConfigObject, Int] => 
      val config = tuple._1
      val resourceIndex = tuple._2
      config.getString("resource_pattern").map { resourcePattern =>
        val methods = config.getStringList("methods").map { x => validateMethods(x.asScala, allowIndex, resourceIndex, config) }.getOrElse(validHttpMethods)
        val headers = config.getStringList("headers").map { _.asScala }.getOrElse(Seq[String]())
        val expose = config.getStringList("expose").map { _.asScala }.getOrElse(Seq[String]())
        val supportsCredentials = config.getBoolean("supports_credentials").getOrElse(false)
        val maxAge = config.getLong("max_age")
        Resource(resourcePattern, methods, headers, expose, supportsCredentials, maxAge)
      }.getOrElse(throw reportError("cors.allow.resources", "resource_pattern must be defined in cors.allow[" + allowIndex + "].resources[" + resourceIndex + "]", config))
    }
  }
  private implicit def configObjectToConfiguration(obj: ConfigObject): Configuration = new Configuration(obj.toConfig())
    
  private val corsConfigExample = """|cors = {
                                     |  allow: [ 
                                     |    {
                                     |      origins: [ "localhost:3000", "127.0.0.1:3000" ],
                                     |      resources: [
                                     |        {
                                     |          resource_pattern: "/file/list_all",
                                     |          headers: [ "x-domain-token" ]
                                     |        },
                                     |        {
                                     |          resource_pattern: "file/at/*",
                                     |          methods: [ "GET", "POST", "PUT", "DELETE", "OPTIONS" ],
                                     |          headers: [ "x-domain-token" ],
                                     |          expose: [ "some-custom-response-header" ],
                                     |          supports_credentials: true,
                                     |          max_age: 500
                                     |        }
                                     |      ]
                                     |    }, 
                                     |    {
                                     |      origins: [ "*" ],
                                     |      resources: [
                                     |        {
                                     |          resource_pattern: "/public/*", 
                                     |          headers: [ "any" ],
                                     |          methods: [ "GET" ]
                                     |        }
                                     |      ]
                                     |    }
                                     |  ]
                                     |}""".stripMargin
  
}

object Cors extends CorsImpl