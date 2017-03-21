name := "CheckoutKata"

version := "1.0"

scalaVersion := "2.12.0"

lazy val scalatest = "org.scalatest" %% "scalatest" % "3.0.1"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  scalatest % Test
)
    