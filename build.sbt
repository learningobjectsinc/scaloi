enablePlugins(Scalac)

scalaVersion := "2.12.10"

normalizedName := "scaloi"
name := "ScaLOI"
description := "Fyne thyngges from Learning Objects, a Cengage Company"

organizationName := "Cengage Learning, Inc."
startYear := Some(2007)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

val sopts = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint:-unused",
  "-Ywarn-unused:imports",
  "-Xfatal-warnings",
  "-Ywarn-unused:imports",
  "-Yno-adapted-args",
  "-language:higherKinds",
  "-language:existentials",
  "-encoding", "UTF-8",
  "-Ypartial-unification",
  "-opt:l:inline",
  "-opt-inline-from:**",
  "-opt-warnings:none",
  "-Ybackend-parallelism", "4",
)

scalacOptions in Compile  ++= sopts
scalacOptions in Test ++= sopts

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.patch)

libraryDependencies ++= Seq(
  "org.http4s"           %% "http4s-client"     % "0.16.6",
  "com.beachape"         %% "enumeratum"        % "1.5.15",
  "com.chuusai"          %% "shapeless"         % "2.3.3",
  "org.scalaz"           %% "scalaz-core"       % "7.2.30",
  "org.scalaz"           %% "scalaz-concurrent" % "7.2.30",
  "org.scalaz.stream"    %% "scalaz-stream"     % "0.8.6",
  "io.argonaut"          %% "argonaut"          % "6.2.3",
)

libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest"                % "3.1.0",
  "org.scalacheck"    %% "scalacheck"               % "1.14.3",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "1.0.0-M2",
  "javax.annotation"   % "javax.annotation-api"     % "1.3.1",
) map (_ % Test)
