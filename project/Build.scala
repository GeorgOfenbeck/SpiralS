import sbt._
import java.io.File

object SpiralSBuild extends Build {

  // -DshowSuppressedErrors=false
  System.setProperty("showSuppressedErrors", "true")



  val scalaTest = "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
  val virtScala = "2.10.2-RC1"
  val bridj = "com.nativelibs4java" % "bridj" % "0.6.1"



  //This is to integrade a build of LMS and a build of the roofline tool - both from github
  object V {
    val roofline_branch = "master"
    val perfplot_branch = "master"
    val lms_branch = "delite-develop-perf"
  }

  object Projects {
    lazy val roofline = RootProject(uri("git://github.com/GeorgOfenbeck/roofline.git#%s".format(V.roofline_branch)))
    lazy val perfplot = RootProject(uri("git://github.com/GeorgOfenbeck/perfplot.git#%s".format(V.perfplot_branch)))
    lazy val lms = RootProject(uri("git://github.com/TiarkRompf/virtualization-lms-core.git#%s".format(V.lms_branch)))
  }

//  .dependsOn(Projects.roofline)
  lazy val spiralS = Project("SpiralS", file("."))
    .dependsOn(Projects.perfplot)
    .dependsOn(Projects.lms)
  //.dependsOn(Projects.roofline)
}
