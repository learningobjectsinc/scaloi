enablePlugins(DECommonSettings)

normalizedName := "scaloi"
name := "ScaLOI"
description := "Generic Functional Data Structures from Learning Objects"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")

libraryDependencies ++= Seq(
  ScalaZ.core,
  ScalaZ.concurrent,
  ScalaZ.stream,
  Testing.scalaTest % "test"
)
