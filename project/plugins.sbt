resolvers += Resolver.url("LO Repo", url("https://learningobjects.jfrog.io/learningobjects/repo"))(Resolver.ivyStylePatterns)

credentials += Credentials(Path.userHome / ".sbt" / "artifactory.credentials")

//https://stash.difference-engine.com/projects/DE/repos/sbt-de-commons/browse
addSbtPlugin("com.learningobjects.sbt" % "sbt-de-commons" % "1.9.6")

//https://github.com/sbt/sbt-release
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")