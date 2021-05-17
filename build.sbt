name := "yoyak"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.5"

crossScalaVersions := List("2.13.5", "2.12.13")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"

libraryDependencies += "org.smali" % "dexlib2" % "2.0.3"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.0-rc1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.0-rc1"

libraryDependencies += "commons-cli" % "commons-cli" % "1.2"

enablePlugins(Antlr4Plugin)

Antlr4 / antlr4PackageName := Some("com.simplytyped.yoyak.parser")
