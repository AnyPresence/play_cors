package com.anypresence.playframework.cors

import org.specs2.mutable._

import play.api.Configuration
import play.api.PlayException
import play.api.test._
import play.api.test.Helpers._

class CorsConfigurationSpec extends Specification {
  
  def fakeApp(config: String) = new FakeApplication() {
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
                                     |    },
                                     |    {
                                     |      origins: [ "/http:\\/\\/.*\\.apple\\.com/" ],
                                     |      resources: [ 
                                     |        {
                                     |          resource_pattern: "/file/list_none"
                                     |        }
                                     |      ]
                                     |    }, 
                                     |    {
                                     |      origins: [ "*" ],
                                     |      resources: [
                                     |        {
                                     |          resource_pattern: "/public/*", 
                                     |          headers: [ "*" ],
                                     |          methods: [ "GET" ]
                                     |        }
                                     |      ]
                                     |    }
                                     |  ]
                                     |}""".stripMargin
  
  "Config reader" should {
    
    "correctly load a valid configuration" in {
      running(fakeApp(corsConfigExample)) {
        val configs = CorsConfigReader.parseConfig
        configs.size must equalTo(3)
        
        val firstCors = configs(0)
        firstCors.origins must equalTo(Seq("localhost:3000", "127.0.0.1:3000"))
        firstCors.resources.size must equalTo(2)
        
        val firstResource = firstCors.resources(0)
        firstResource.resourcePattern must equalTo("/file/list_all")
        firstResource.methods must equalTo(Seq("GET", "PUT", "POST", "DELETE", "TRACE", "CONNECT", "OPTIONS", "HEAD"))
        firstResource.expose must equalTo(Seq[String]())
        firstResource.supportsCredentials must equalTo(false)
        firstResource.maxAge must beNone
        
        val secondResource = firstCors.resources(1)
        secondResource.resourcePattern must equalTo("file/at/*")
        secondResource.methods must equalTo(Seq("GET", "POST", "PUT", "DELETE", "OPTIONS"))
        secondResource.headers.size must equalTo(1)
        secondResource.headers(0).toUpperCase must equalTo("X-DOMAIN-TOKEN")
        secondResource.expose must equalTo(Seq("some-custom-response-header"))
        secondResource.supportsCredentials must equalTo(true)
        secondResource.maxAge must beSome.which(_ == 500)
        
        val secondCors = configs(1)
        secondCors.origins must equalTo(Seq("/http:\\/\\/.*\\.apple\\.com/"))
        secondCors.resources.size must equalTo(1)
        
        val theResource = secondCors.resources(0)
        theResource.resourcePattern must equalTo("/file/list_none")
        theResource.methods must equalTo(Seq("GET", "PUT", "POST", "DELETE", "TRACE", "CONNECT", "OPTIONS", "HEAD"))
        theResource.expose must equalTo(Seq[String]())
        theResource.supportsCredentials must equalTo(false)
        theResource.maxAge must beNone
        
        val thirdCors = configs(2)
        thirdCors.origins must equalTo(Seq("*"))
        thirdCors.resources.size must equalTo(1)
        
        val lastResource = thirdCors.resources(0)
        lastResource.resourcePattern must equalTo("/public/*")
        lastResource.headers.size must equalTo(1)
        lastResource.headers(0) must equalTo("*")
        lastResource.methods must equalTo(Seq("GET"))
        lastResource.expose must equalTo(Seq[String]())
        lastResource.supportsCredentials must equalTo(false)
        lastResource.maxAge must beNone
      }
    }
    
    "should fail if resource_pattern is missing on a resource definition" in {
      
      val configText = """|cors = {
                          |  allow: [ 
                          |    {
                          |      origins: [ "*" ],
                          |      resources: [
                          |        {
                          |          headers: [ "*" ],
                          |          methods: [ "GET" ]
                          |        }
                          |      ]
                          |    }
                          |  ]
                          |}""".stripMargin
                          
      running(fakeApp(configText)) {
        CorsConfigReader.parseConfig must throwA[PlayException]
      }
    }
    
    "should fail if an invalid method is specified" in {
      val configText = """|cors = {
                          |  allow: [ 
                          |    {
                          |      origins: [ "*" ],
                          |      resources: [
                          |        {
                          |          resource_pattern: "*",  
                          |          headers: [ "*" ],
                          |          methods: [ "SPLORT" ]
                          |        }
                          |      ]
                          |    }
                          |  ]
                          |}""".stripMargin
      
      running(fakeApp(configText)) {
        CorsConfigReader.parseConfig must throwA[PlayException]
      }
    }
    
    "should fail if no origins are specified" in {
      val configText = """|cors = {
                          |  allow: [ 
                          |    {
                          |      resources: [
                          |        {
                          |          resource_pattern: "*",  
                          |          headers: [ "*" ],
                          |          methods: [ "SPLORT" ]
                          |        }
                          |      ]
                          |    }
                          |  ]
                          |}""".stripMargin
      
      running(fakeApp(configText)) {
        CorsConfigReader.parseConfig must throwA[PlayException]
      }
    }
    
    "should fail if no resources are specified" in {
      val configText = """|cors = {
                          |  allow: [ 
                          |    {
                          |      origins: [ "*" ]                            
                          |    }
                          |  ]
                          |}""".stripMargin
      
      running(fakeApp(configText)) {
        CorsConfigReader.parseConfig must throwA[PlayException]
      }
    }
    
    "should fail if origin starts with /, ends with /, but cannot be parsed as a valid regex" in {
      val configText = """|cors = {
                          |  allow: [ 
                          |    {
                          |      origins: [ "/*/" ],
                          |      resources: [
                          |        {
                          |          resource_pattern: "*",  
                          |          headers: [ "*" ],
                          |          methods: [ "SPLORT" ]
                          |        }
                          |      ]
                          |    }
                          |  ]
                          |}""".stripMargin
      running(fakeApp(configText)) {
        CorsConfigReader.parseConfig must throwA[PlayException]
      }
       
    }
    
  }
}