# Play CORS module

To use Cross Origin Resource Sharing (CORS) in your application, follow the steps below to configure the Play CORS module.

## Configure CORS settings

You'll need to configure CORS settings similarly to the following example.

  cors = {
    allow: [ 
      {
        origins: [ "localhost:3000", "127.0.0.1:3000" ],
        resources: [
          {
            resource_pattern: "/file/list_all",
            headers: [ "x-domain-token" ]
          },
          {
            resource_pattern: "file/at/*",
            methods: [ "GET", "POST", "PUT", "DELETE", "OPTIONS" ],
            headers: [ "x-domain-token" ],
            expose: [ "some-custom-response-header" ],
            supports_credentials: true,
            max_age: 500
          }
        ]
      }, 
      {
        origins: [ "*" ],
        resources: [
          {
            resource_pattern: "/public/*", 
            headers: [ "any" ],
            methods: [ "GET" ]
          }
        ]
      }
    ]
  }

## Mix cors filter into Global

Add the corsFilter filter to your Global object in the base package of your application. If you don't already have a Global.scala in the base package of your app, you'll need to create one.

Below is an example of a simple Global.scala file, that simply employs the CORS filter to enable CORS support.

  import com.anypresence.playframework.cors.Cors
  import play.api.GlobalSettings
  import play.api.mvc.WithFilters

  object Global extends WithFilters(Cors.corsFilter) 