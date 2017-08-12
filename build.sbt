name := "yoyak"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"


libraryDependencies += "org.smali" % "dexlib2" % "2.0.3"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.0-rc1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.0-rc1"

libraryDependencies += "commons-cli" % "commons-cli" % "1.2"

enablePlugins(Antlr4Plugin)

antlr4PackageName in Antlr4 := Some("com.simplytyped.yoyak.parser")
