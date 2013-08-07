// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers ++= Seq(
	"Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
	"fuzion24-releases" at "http://fuzion24.github.io/maven/releases"
)

addSbtPlugin("play" % "sbt-plugin" % "2.1.3")

addSbtPlugin("com.github.hexx" % "sbt-github-repo" % "0.1.0")

