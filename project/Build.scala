import sbt._
import Keys._
import play.Project._
import com.github.hexx.GithubRepoPlugin._

object ApplicationBuild extends Build {

  val appName         = "play_cors"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(

    localRepo := Path.userHome / "github" / "play_api_maven_repo",

    githubRepo := "git@github.com:AnyPresence/play_api_maven_repo.git"

  ).settings(githubRepoSettings: _*)

}
