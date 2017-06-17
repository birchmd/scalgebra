lazy val root = (project in file("."))
  .settings(
    name := "Scalgebra",
    scalaVersion := "2.12.2"
  )
  
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"