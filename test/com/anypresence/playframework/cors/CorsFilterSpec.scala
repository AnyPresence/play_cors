package com.anypresence.playframework.cors

import org.specs2.mutable._

import play.api.Configuration
import play.api.GlobalSettings
import play.api.PlayException
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.Results._
import play.api.mvc.WithFilters
import play.api.test._
import play.api.test.Helpers._


class CorsFilterSpec extends Specification {
  
  object Global extends WithFilters(Cors.corsFilter)
  
  object FakeController extends Controller { 
    def index = Action {
      Ok("Hello world")
    }
  }
   
  def fakeApp(config: String) = new FakeApplication(withGlobal = Some(Global)) {
    override def configuration = new Configuration(com.typesafe.config.ConfigFactory.parseString(config))
  }
  
  private val corsConfigExample = """|cors = {
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
                                     |    }
                                     |  ]
                                     |}""".stripMargin
    
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
    
  val simpleRequest = FakeRequest("GET", "/file/at/some_file.txt").withHeaders(ORIGIN -> "localhost:3000", CONTENT_TYPE -> "multipart/form-data")
    
  "Cors filter" should {
    
    running(fakeApp(corsConfigExample)) {
    
      "respond to simple request" in {
      
        "omit CORS response headers when Origin header is not present" in {
          val result = FakeController.index(FakeRequest())
          header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
          header(ACCESS_CONTROL_EXPOSE_HEADERS, result) must beNone
          header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
        }
        
        "omit CORS response headers when Origin request header doesn't match configured allowable origins" in {
          val result = FakeController.index(FakeRequest().withHeaders(ORIGIN -> "localhost:3001"))
          header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
          header(ACCESS_CONTROL_EXPOSE_HEADERS, result) must beNone
          header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
        }
        
        /*"add the Access-Control-Allow-Origin header if the resource supports credentials" in {
          val result = FakeController.index(simpleRequest)
          header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beSome.which(_ == "true")
          //header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
          //header(ACCESS_CONTROL_EXPOSE_HEADERS, result) must beNone
          //header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone
        }*/
        
      }
      
      "respond to preflight request" in {

        "omit CORS response headers when Origin header is not present" in {
          val result = FakeController.index(FakeRequest())
          header(ACCESS_CONTROL_ALLOW_ORIGIN, result) must beNone
          header(ACCESS_CONTROL_ALLOW_CREDENTIALS, result) must beNone          
          header(ACCESS_CONTROL_MAX_AGE, result) must beNone
          header(ACCESS_CONTROL_ALLOW_METHODS, result) must beNone
          header(ACCESS_CONTROL_ALLOW_HEADERS, result) must beNone
        }

      }

      /*"correctly respond to malformed request" in {

      }*/
      
    }
    
  }
  
}