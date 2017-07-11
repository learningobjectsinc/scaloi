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
