import sbt._
import Keys._

object ApplicationBuild extends Build {

  val appName = "tracr"

  val localRepo = Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
  )

  val sonatypeRepo = Seq(
    "Sonatype repositosnapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype repository releases" at "http://oss.sonatype.org/content/repositories/releases/"
  )

  val mandubianRepo = Seq(
    "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"
  )

  lazy val tracrProject = Project(
    appName, file("."),
    settings = Project.defaultSettings ++ Seq(
      fork := true,
      scalaVersion := "2.10.2",
      javaOptions in run += "-Xmx8G",
      resolvers ++= localRepo ++ sonatypeRepo ++ mandubianRepo,
      libraryDependencies ++= Seq(
        "play" %% "play-json" % "2.2-SNAPSHOT",
        "org.eclipse.imp" % "org.eclipse.imp.pdb.values" % "0.4.1.qualifier",
        "org.specs2" %% "specs2" % "1.13" % "test",
        "junit" % "junit" % "4.11" % "test"
      )
    )
  )
}
