normalizedName := "scaloi"
name := "ScaLOI"
description := "Generic Functional Data Structures from Learning Objects"

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")
scalaVersion in ThisBuild := "2.11.8"

lazy val scaloiz = (project in file("scaloiz"))
  .enablePlugins(DECommonSettings)
  .settings(
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),
    normalizedName := "ScaloiZ",
    description := "Functional bells and whistles for ScalaZ",
    libraryDependencies ++= Seq(
      ScalaZ.core,
      ScalaZ.concurrent,
      Testing.scalaTest % "test"
    )
  )
