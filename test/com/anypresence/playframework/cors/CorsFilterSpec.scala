package com.anypresence.playframework.cors

import org.specs2.mutable._

import play.api.Configuration
import play.api.GlobalSettings
import play.api.PlayException
import play.api.libs.ws.WS
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.Handler
import play.api.mvc.RequestHeader
import play.api.mvc.Results._
import play.api.mvc.WithFilters
import play.api.test._
import play.api.test.Helpers._

class CorsTest extends CorsImpl

class Global extends WithFilters(new CorsTest().corsFilter) 

//
// a simple fake controller
//

class CorsFilterSpec extends Specification {
  
  private val corsConfigExample = """|cors = {
                                     |  allow: [ 
                                     |    {
                                     |      origins: [ "localhost:3000", "127.0.0.1:3000" ],
                                     |      resources: [
                                     |        {
                                     |          resource_pattern: "/file/doesnt_support_creds",
                                     |          supports_credentials: false
                                     |        },
                                     |        {
                                     |          resource_pattern: "/file/list_all"
                                     |        },
                                     |        {
                                     |          resource_pattern: "/file/at/*",
                                     |          methods: [ "GET", "PUT", "DELETE", "OPTIONS" ],
                                     |          headers: [ "X-Domain-Token" ],
                                     |          expose: [ "X-Custom-Response-Header", "X-Custom-Response-Other-Header" ],
                                     |          supports_credentials: true,
                                     |          max_age: 500
                                     |        }
                                     |      ]
                                     |    }
                                     |  ] 
                                     |}""".stripMargin 
  private val configObject: Configuration = new Configuration(com.typesafe.config.ConfigFactory.parseString(corsConfigExample))
  private val configExample = configObject.getObject("cors").get
  private def fakeApp = new FakeApplication(withGlobal = Some(new Global), additionalConfiguration = Map("cors" -> configExample.unwrapped()))
  
                                     
  private val corsConfigWithCatchallOriginExample = """|cors = {
                                     |  allow: [ 
                                     |    {
                                     |      origins: [ "localhost:3000", "127.0.0.1:3000" ],
                                     |      resources: [
                                     |        {
                                     |          resource_pattern: "/file/list_all"
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
  private val fallbackConfigObject: Configuration = new Configuration(com.typesafe.config.ConfigFactory.parseString(corsConfigWithCatchallOriginExample))
  private val fallbackConfigExample = fallbackConfigObject.getObject("cors").get
  private def fallbackFakeApp = new FakeApplication(withGlobal = Some(new Global), additionalConfiguration = Map("cors" -> fallbackConfigExample.unwrapped()))
                                     
  "Cors filter" should {
    
    "respond to simple request" in {
      
      "omit CORS response headers when Origin header is not present" in new WithApplication(fakeApp) {
        play.api.Logger.info ("corsConfigExample: " + configExample)
        val result = route(FakeRequest("GET", "/index.html")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
        header(ACCESS_CONTROL_EXPOSE_HEADERS, result) must beNone
        header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
      }
        
      "omit CORS response headers when Origin request header doesn't match configured allowable origins" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/index.html").withHeaders(ORIGIN -> "localhost:3001")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
        header(ACCESS_CONTROL_EXPOSE_HEADERS, result) must beNone
        header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
      }
      
      "add the Access-Control-Allow-Origin header if the resource supports credentials" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome.which(_ == "localhost:3000")
        header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beSome.which(_ == "true")
      }
      
      "omit Access-Control-Allow-Crerdentials header if the resource does not specify supports credentials" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome.which(_ == "*")
        header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
      }
      
      "omit Access-Control-Allow-Crerdentials header if the resource specifies that it does not support credentials" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/doesnt_support_creds").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome.which(_ == "*")
        header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
      }
      
      "include exposed response headers if configured" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
        header(ACCESS_CONTROL_EXPOSE_HEADERS, result) must beSome.which (_ == "X-Custom-Response-Header,X-Custom-Response-Other-Header" )
      }
      
      "omit exposed response headers if not configured" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        play.api.Logger.info("FART : " + headers(result))
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
        header(ACCESS_CONTROL_EXPOSE_HEADERS, result) must beNone
      }
      
      "using GET method" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using HEAD method" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("HEAD", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using POST method" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("POST", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Content-Type of text/plain" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Content-Type of text/plain with case insensitivity" in new WithApplication(fakeApp) { 
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "TEXT/PLAIN")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Content-Type of application/x-www-form-urlencoded" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "application/x-www-form-urlencoded")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Content-Type of multipart/form-data" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "multipart/form-data")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Content-Type header case insensitive" in new WithApplication(fakeApp) { 
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", "CONTENT-TYPE" -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Accept header" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain", ACCEPT -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Accept header case insensitive" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain", "ACCEPT" -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Accept-Language header" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain", ACCEPT_LANGUAGE -> "en")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Accept-Language header case insensitive" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain", "ACCEPT-LANGUAGE" -> "en")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Content-Language header" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain", CONTENT_LANGUAGE -> "en")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using Content-Language header case insensitive" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain", "CONTENT-LANGUAGE" -> "en")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
    }
    
    "not respond with Origin header to request that is not valid simple request or valid preflight request" in {
      
      "using valid simple request as basline" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "using non-simple header" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain", "X-Custom-Header" -> "Some value")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
      "using non-simple method" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("DELETE", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "text/plain")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
      "using non-simple content type" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("GET", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "application/json")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
    }
    
    "be configured to" in {
      
      "Use origin * when requested origin is not found" in new WithApplication(fallbackFakeApp) {
        val result = route(FakeRequest("OPTIONS", "/public/cats").withHeaders(ORIGIN -> "www.somefakedomain.com:3000", ACCESS_CONTROL_REQUEST_METHOD -> "GET")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
        header(ACCESS_CONTROL_ALLOW_HEADERS, result) must beSome.which(_ == "any")
      }
      
      "Return a 404 when no route exists for the object of the preflight request" in new WithServer(fakeApp, 3333) {
        val result = route(FakeRequest("OPTIONS", "/public/some_invalid_url").withHeaders(ORIGIN -> "www.somefakedomain.com:3000", ACCESS_CONTROL_REQUEST_METHOD -> "GET")).get
        status(result) must be equalTo(404)
      }
      
    }
    
    "respond to preflight request" in {
      
      "omit CORS headers if origin is not present in the request" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
      "omit CORS headers if origin does not match" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3001", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
      "omit CORS headers if origin is a case insensitive match" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "LOCALHOST:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
      "omit CORS headers if Access-Control-Request-Method header is not present" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
      "omit CORS headers if Access-Control-Request-Method header is not in the list of allowed methods" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "POST")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }

      "omit CORS headers if Access-Control-Request-Method header is in the list of allowed methods, but is case insensitive match" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "put")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
      "omit CORS headers if Access-Control-Request-Headers header contains header not in allowed list" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT", ACCESS_CONTROL_REQUEST_HEADERS -> "X-Custom-Header-That-Is-Not-In-Configured-List")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
      }
      
      "include CORS headers if Access-Control-Request-Headers header contains header in allowed list" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT", ACCESS_CONTROL_REQUEST_HEADERS -> "X-Domain-Token")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }
      
      "include CORS headers if Access-Control-Request-Headers header contains header in allowed list, but not correct case" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT", ACCESS_CONTROL_REQUEST_HEADERS -> "X-DOMAIN-TOKEN")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
      }

      "include Access-Control-Allow-Credentials with true if configured for credentials" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome.which(_ == "localhost:3000")
        header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beSome.which(_ == "true")
      }
      
      "omit Access-Control-Allow-Credentials if explicitly not configured for credentials" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/doesnt_support_creds").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome.which(_ == "*")
        header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
      }
      
      "omit Access-Control-Allow-Credentials if implicitly not configured for credentials" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome.which(_ == "*")
        header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
      }
      
      "include Access-Control-Max-Age header for a resource that has max age configured" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
        header(ACCESS_CONTROL_MAX_AGE, result) must beSome.which(_ == "500")
      }
      
      "omit Access-Control-Max-Age header for a resource that does not have max age configured" in new WithApplication(fakeApp) { 
        val result = route(FakeRequest("OPTIONS", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
        header(ACCESS_CONTROL_MAX_AGE, result) must beNone
      }
      
      "include Access-Control-Allow-Methods header for all methods if none are configured" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
        header(ACCESS_CONTROL_ALLOW_METHODS, result) must beSome.which(_ == "GET, PUT, DELETE, OPTIONS")
      }
      
      "include Access-Control-Allow-Methods header for only configured methods" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/list_all").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
        header(ACCESS_CONTROL_ALLOW_METHODS, result) must beSome.which(_ == "GET, PUT, POST, DELETE, TRACE, CONNECT, OPTIONS, HEAD")
      }
      
      "include Access-Control-Allow-Headers configured for a resource" in new WithApplication(fakeApp) {
        val result = route(FakeRequest("OPTIONS", "/file/at/1.html").withHeaders(ORIGIN -> "localhost:3000", ACCESS_CONTROL_REQUEST_METHOD -> "PUT")).get
        header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beSome
        header(ACCESS_CONTROL_ALLOW_HEADERS, result) must beSome.which(_ == "X-Domain-Token")
      }
      
    }
    
  }
  
}
