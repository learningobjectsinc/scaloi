resolvers ++= Seq(
  Resolver.url("LO Repo", url("https://learningobjects.artifactoryonline.com/learningobjects/repo"))(Resolver.ivyStylePatterns)
)

//https://stash.difference-engine.com/projects/DE/repos/sbt-de-commons/browse
addSbtPlugin("com.learningobjects.sbt" % "sbt-de-commons" % "1.6.4")

//https://github.com/sbt/sbt-release
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")

