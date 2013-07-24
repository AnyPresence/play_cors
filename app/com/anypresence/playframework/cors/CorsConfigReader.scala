package com.anypresence.playframework.cors

import com.typesafe.config.ConfigObject

import play.api.Configuration
import play.api.Play.current
import play.api.Logger._

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

case class CorsConfig(origins: Seq[String], resources: Seq[Resource])
case class Resource(resourcePattern: String, methods: Seq[String], headers: Seq[String], expose: Seq[String], supportsCredentials: Boolean, maxAge: Option[Long])

object CorsConfigReader {
  
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
  
  private def parseResources(resources: Seq[ConfigObject], allowIndex: Int): Seq[Resource] = {
    resources.zipWithIndex.map { tuple: Tuple2[ConfigObject, Int] => 
      val config = tuple._1
      val resourceIndex = tuple._2
      config.getString("resource_pattern").map { resourcePattern =>
        val methods = config.getStringList("methods").map { _.asScala }.getOrElse(Seq[String]("GET", "PUT", "POST", "DELETE", "TRACE", "CONNECT", "OPTIONS", "HEAD"))
        val headers = config.getStringList("headers").map { _.asScala }.getOrElse(Seq[String]()).map { _.toUpperCase() }
        val expose = config.getStringList("expose").map { _.asScala }.getOrElse(Seq[String]())
        val supportsCredentials = config.getBoolean("supports_credentials").getOrElse(true)
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