import sbt._
import Keys._

object FPInScalaBuild extends Build {
  val opts = Project.defaultSettings ++ Seq(
    scalaVersion := "2.11.6",
    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      "bintray/non" at "http://dl.bintray.com/non/maven",
      "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
    ),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "3.4" % "test",
      "org.specs2" %% "specs2-scalacheck" % "3.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2"),
    // The scalacOptions are reset for tests with this command
    // because it doesn't seem to be possible through ivy to scope
    // to exclude a compiler plugin from the unit tests
    // and kind-projector conflicts with -Yrangepos
    scalacOptions in Test := Seq("-Yrangepos")
  )

  lazy val root =
    Project(id = "fpinscala",
            base = file("."),
            settings = opts ++ Seq(
              onLoadMessage ~= (_ + nio2check())
            )) aggregate (chapterCode, exercises, answers)
  lazy val chapterCode =
    Project(id = "chapter-code",
            base = file("chaptercode"),
            settings = opts)
  lazy val exercises =
    Project(id = "exercises",
            base = file("exercises"),
            settings = opts)
  lazy val answers =
    Project(id = "answers",
            base = file("answers"),
            settings = opts)

  def nio2check(): String = {
    val cls = "java.nio.channels.AsynchronousFileChannel"
    try {Class.forName(cls); ""}
    catch {case _: ClassNotFoundException =>
      ("\nWARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.\n" +
       "You are probably running Java < 1.7; answers will not compile.\n" +
       "You seem to be running " + System.getProperty("java.version") + ".\n" +
       "Try `project exercises' before compile, or upgrading your JDK.")
    }
  }
}
