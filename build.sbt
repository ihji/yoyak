name := "yoyak"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.7"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"

libraryDependencies += "org.smali" % "dexlib2" % "2.0.3"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.15.0"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.15.0"

libraryDependencies += "commons-cli" % "commons-cli" % "1.5.0"

enablePlugins(Antlr4Plugin)

Antlr4 / antlr4PackageName := Some("com.simplytyped.yoyak.parser")
