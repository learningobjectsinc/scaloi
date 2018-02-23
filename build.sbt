
enablePlugins(DERepositories)
enablePlugins(DECommonSettings, Release)

normalizedName := "scaloi"
name := "ScaLOI"
description := "Generic Functional Utilities from Learning Objects"

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)
addCompilerPlugin(ScalaExtensions.macroParadise)

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-client" % "0.15.2a",
  ScalaExtensions.enumeratum,
  ScalaExtensions.shapeless,
  ScalaExtensions.simulacrum,
  ScalaZ.concurrent(),
  ScalaZ.core(),
  ScalaZ.stream(),
)

libraryDependencies ++= Seq(
  JSON.Argonaut.argonaut,
  Testing.scalaTest,
  "org.scalacheck"  %% "scalacheck" % "1.13.4",
) map (_ % Test)
