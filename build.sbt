enablePlugins(Scalac)

scalaVersion := "2.12.7"

normalizedName := "scaloi"
name := "ScaLOI"
description := "Fyne thyngges from Learning Objects, a Cengage Company"

organizationName := "Cengage Learning, Inc."
startYear := Some(2007)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.scalamacros"  % "paradise"       % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.http4s"           %% "http4s-client"     % "0.15.2a",
  "com.beachape"         %% "enumeratum"        % "1.5.13",
  "com.chuusai"          %% "shapeless"         % "2.3.2",
  "com.github.mpilquist" %% "simulacrum"        % "0.12.0",
  "org.scalaz"           %% "scalaz-core"       % "7.2.24",
  "org.scalaz"           %% "scalaz-concurrent" % "7.2.24",
  "org.scalaz.stream"    %% "scalaz-stream"     % "0.8.6a",
  "io.argonaut"          %% "argonaut"          % "6.2",
)

libraryDependencies ++= Seq(
  "org.scalatest"   %% "scalatest"  % "3.0.1",
  "org.scalacheck"  %% "scalacheck" % "1.13.4",
) map (_ % Test)

