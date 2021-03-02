enablePlugins(Scalac)

scalaVersion := "2.12.13"

normalizedName := "scaloi"
name := "ScaLOI"
description := "Fyne thyngges from Learning Objects, an LO Venture"

organization := "com.learningobjects"
homepage := Some(url("https://github.com/learningobjectsinc/scaloi"))
scmInfo := Some(ScmInfo(url("https://github.com/learningobjectsinc/scaloi"), "git@github.com:learningobjectsinc/scaloi.git"))
organizationName := "Learning Objects"
startYear := Some(2007)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
developers := List(
  Developer("", "Merlin Hughes", "", null),
  Developer("", "Harrison Houghton", "", null),
  Developer("", "Zack Powers", "", null),
  Developer("", "Keith Wooldridge", "", null),
  Developer("", "Stephen Jordan", "", null),
  Developer("", "Syed Jafri", "", null),
  Developer("", "Paul Gray", "", null),
  Developer("", "Jasna Blemberg", "", null),
  Developer("", "Michael Kalish", "", null),
  Developer("", "Nick Van Aartsen", "", null),
  Developer("", "Steven Burt", "", null),
  Developer("", "Hunter Savage", "", null),
  Developer("", "Michael Kalish", "", null),
)

publishMavenStyle := true

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

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

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.patch)

libraryDependencies ++= Seq(
  "com.beachape"         %% "enumeratum"        % "1.6.1",
  "com.chuusai"          %% "shapeless"         % "2.3.3",
  "org.scalaz"           %% "scalaz-core"       % "7.2.31",
  "org.scalaz"           %% "scalaz-concurrent" % "7.2.31",
  "io.argonaut"          %% "argonaut"          % "6.3.3",
)

libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest"                % "3.2.5",
  "org.scalacheck"    %% "scalacheck"               % "1.15.3",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "1.0.0-M2",
  "javax.annotation"   % "javax.annotation-api"     % "1.3.2",
) map (_ % Test)

import ReleaseTransformations._

releaseCrossBuild := true

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeRelease"),
  pushChanges
)
