package build

import com.typesafe.tools.mima.plugin.{MimaKeys, MimaPlugin}
import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin

object Release extends AutoPlugin {
  override val requires = ReleasePlugin && MimaPlugin
  override val trigger = noTrigger

  import MimaKeys._
  import ReleasePlugin.autoImport._
  import ReleaseTransformations._

  override lazy val projectSettings: Seq[Def.Setting[_]] = Seq(
    publishTo in ThisBuild := {
      if (!isSnapshot.value)
        Some("LO Misc" at "https://learningobjects.jfrog.io/learningobjects/lo-misc")
      else
        Some("LO Misc" at "https://learningobjects.jfrog.io/learningobjects/lo-misc;build.timestamp=" + new java.util.Date().getTime)
    },
    /* scaladoc? we don't need no stinkin' scaladoc (yet) */
    sources in doc in Compile := Nil,
    publishArtifact in packageDoc := false,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      releaseStepTask(mimaReportBinaryIssues),
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
    mimaPreviousArtifacts := {
      val rawVersion = version.value
      val snapshot   = isSnapshot.value
      val group      = organization.value
      val module     = normalizedName.value
      val crossing   = crossPaths.value

      val prefix = if (crossing) group %% module else group % module // wow.

      val ver = if (snapshot) rawVersion.dropRight("-SNAPSHOT".length) else rawVersion

      ver.split('.') match {
        case Array(Num(maj), Num(min), Num(pat)) if pat > 0 =>
          Set(prefix % s"$maj.$min.${pat - 1}")
        case Array(Num(maj), Num(min), Num(pat)) =>
          Set.empty
        case _ =>
          println {
            s"!*!*! Unable to parse version string $rawVersion for $group:$module! MiMa will not run. !*!*!"
          }
          Set.empty
      }
    }
  )

  private object Num {
    def unapply(arg: String): Option[Int] =
      try Some(arg.toInt) catch { case _ : NumberFormatException => None}
  }

}
