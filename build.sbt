import com.learningobjects.sbt.libraries.JSON.Argonaut

enablePlugins(DERepositories)

normalizedName := "scaloi"
name := "ScaLOI"
description := "Generic Functional Data Structures from Learning Objects"

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

lazy val core = (project in file("core"))
  .enablePlugins(DECommonSettings, Release)
  .settings(
    normalizedName := "scaloi-core",
    name := "scaloi-core",
    description := "Yet Another Core project."
  )

lazy val scaloiz = (project in file("scaloiz"))
  .enablePlugins(DECommonSettings, Release)
  .settings(
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),
    normalizedName := "scaloiz",
    name := "scaloiz",
    description := "Functional bells and whistles for ScalaZ",
    libraryDependencies ++= Seq(
      ScalaZ.core,
      ScalaZ.concurrent,
      ScalaZ.stream,
      Testing.scalaTest % "test"
    )
  )

lazy val freemium = (project in file("freemium"))
  .enablePlugins(DECommonSettings, Release)
  .dependsOn(scaloiz)
  .settings(
    addCompilerPlugin(ScalaExtensions.kindProjector),
    normalizedName := "scaloi-freemium",
    name := "Freemium - Utility APIs implemented with the Free Monad.",
    libraryDependencies ++= Seq(
      Testing.scalaTest % "test"
    )
  )

lazy val putty = (project in file("putty"))
  .enablePlugins(DECommonSettings, Release)
  .settings(
    addCompilerPlugin(ScalaExtensions.macroParadise),
    normalizedName := "scaloi-putty",
    name := "Putty - Giving shape to that which is shapeless.",
    libraryDependencies ++= Seq(
      ScalaExtensions.shapeless,
      ScalaExtensions.simulacrum,
      Testing.scalaTest % "test"
    )
  )

lazy val http4Steve = (project in file("http4s"))
  .enablePlugins(DECommonSettings, Release)
  .settings(
    normalizedName := "http4stephen",
    name := "http4stephen - Useful extras for http4s.",
    libraryDependencies ++=Seq(
      "org.http4s" %% "http4s-client" % "0.15.2a",
      Argonaut.argonaut % "test",
      Testing.scalaTest % "test"
    )
  )
