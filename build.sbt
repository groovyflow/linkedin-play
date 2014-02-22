name := "linkedin-play"

version := "1.0-SNAPSHOT"


scalaVersion := "2.10.2"

resolvers += Classpaths.sbtPluginReleases

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Typesafe snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"

resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

libraryDependencies ++= {
  val playVersion = "2.2.1"
  Seq(
    "com.typesafe.play" %% "play" % playVersion,
    "com.typesafe.play" %% "play-slick" % "0.6.0.1" ,
    "com.typesafe.play" %% "play-jdbc" % playVersion,
    "com.typesafe.slick" %% "slick" % "2.0.0",
    "javax.servlet" % "javax.servlet-api" % "3.0.1", //needed by org.reflections
    "com.google.code.findbugs" % "jsr305" % "2.0.1", //needed by org.reflections
    ("org.reflections" % "reflections" % "0.9.8" notTransitive())
      .exclude("com.google.guava", "guava") //provided by play
      .exclude("javassist", "javassist"), //provided by play
    "org.hsqldb" % "hsqldb" % "2.3.1" % "test",
    "com.typesafe.play" %% "play-test" % playVersion % "test")
}



     

play.Project.playScalaSettings
