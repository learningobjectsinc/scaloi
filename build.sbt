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
    description := "Yet Another Core project.",
    libraryDependencies ++= Seq(
      Testing.scalaTest % "test",
      "org.scalacheck"  %% "scalacheck" % "1.13.4" % "test"
    )
  )

lazy val scaloiz = (project in file("scaloiz"))
  .enablePlugins(DECommonSettings, Release)
  .dependsOn(core)
  .settings(
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),
    normalizedName := "scaloiz",
    name := "scaloiz",
    description := "Functional bells and whistles for ScalaZ",
    libraryDependencies ++= Seq(
      ScalaZ.core(),
      ScalaZ.concurrent(),
      ScalaZ.stream(),
      Testing.scalaTest % "test",
      "org.scalacheck"  %% "scalacheck" % "1.13.4" % "test"
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
  .dependsOn(core)
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

lazy val enumeratumZed = (project in file("enumeratumZed"))
  .enablePlugins(DECommonSettings, Release)
  .settings(
    normalizedName := "enumeratumZed",
    name := "EnumeratumZed - Scalaz-Enumeratum integrations",
    libraryDependencies ++= Seq(
      ScalaZ.core(),
      ScalaExtensions.enumeratum,
      Testing.scalaTest % "test"
    )
  )

lazy val niceThings = (project in file("niceThings"))
  .enablePlugins(DECommonSettings, Release)
  .dependsOn(core, scaloiz, freemium, putty, enumeratumZed)
  .settings(
    addCompilerPlugin(ScalaExtensions.macroParadise),
    normalizedName := "scaloi-nicethings",
    name := "This is why we CAN have nice things.",
    libraryDependencies ++= Seq(
      ScalaExtensions.shapeless,
      ScalaExtensions.simulacrum,
      Testing.scalaTest % "test"
    )
  )
