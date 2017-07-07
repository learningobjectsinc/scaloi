enablePlugins(DERepositories)

normalizedName := "scaloi"
name := "ScaLOI"
description := "Generic Functional Data Structures from Learning Objects"

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

lazy val `scaloi-core` = (project in file("scaloi-core"))
  .enablePlugins(DECommonSettings)
  .settings(
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),
    normalizedName := "scaloi-core",
    name := "scaloi-core",
    description := "Functional utilities for Scala",
    libraryDependencies ++= Seq(
      ScalaExtensions.shapeless,
      ScalaZ.core,
      ScalaZ.concurrent,
      ScalaZ.stream,
      Testing.scalaTest % "test"
    )
  )

lazy val freemium = (project in file("freemium"))
  .enablePlugins(DECommonSettings)
  .dependsOn(`scaloi-core`)
  .settings(
    addCompilerPlugin(ScalaExtensions.kindProjector),
    normalizedName := "scaloi-freemium",
    name := "Freemium - Utility APIs implemented with the Free Monad.",
    libraryDependencies ++= Seq(
      Testing.scalaTest % "test"
    )
  )

lazy val putty = (project in file("putty"))
  .enablePlugins(DECommonSettings)
  .settings(
    addCompilerPlugin(ScalaExtensions.macroParadise),
    normalizedName := "scaloi-putty",
    name := "Putty - Giving shape to that which is shapeless.",
    libraryDependencies ++= Seq(
      ScalaExtensions.shapeless,
      ScalaExtensions.simulacrum
    )
  )
