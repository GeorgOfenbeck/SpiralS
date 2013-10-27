name := "SpiralS"

version := "0.4-SNAPSHOT"

organization := "ETHZ"

resolvers += ScalaToolsSnapshots

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)

// needed for scala.tools, which is apparently not included in sbt's built in version
libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % virtScala

libraryDependencies += "org.scala-lang" % "scala-actors" % virtScala // for ScalaTest

libraryDependencies += scalaTest

libraryDependencies += bridj

libraryDependencies += "com.github.rwl" % "jtransforms" % "2.4.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "EPFL" % "lms_2.10" % "0.3.1-SNAPSHOT"

libraryDependencies += "ETHZ" % "perfplot_2.10" % "1.2"

libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.7.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

libraryDependencies += "com.typesafe.slick" %% "slick" % "1.0.1"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"

libraryDependencies += "net.java.dev.jna" % "jna" % "3.4.0"

libraryDependencies += "pl.project13.scala" %% "rainbow" % "0.1"

libraryDependencies += "com.chuusai" %% "shapeless" % "1.2.4"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % virtScala)


scalacOptions += "-P:continuations:enable"

fork in run := true


initialCommands in console := """
ch.ethz.spirals.db.DB.checkCreate() //check if DB exits - otherwise create
import ch.ethz.spirals.search.Search._
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.db.Queries._
println("     _______  _______  ___   __      ____     Automatic              \n    / __/ _ \\/  _/ _ \\/ _ | / /     / __/     * Implementation       \n   _\\ \\/ ___// // , _/ __ |/ /__   _\\ \\       * Optimization         \n  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation   \n                                              of DSP Algorithms       \n  https://bitbucket.org/GeorgOfenbeck/spirals\n  SpiralS 0.1 Prototype\n----------------------------------------------------------  ")
"""
