//Configs
name := "LCVQE"

version := "0.1"

scalaVersion := "2.12.3"

//Resolvers
resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += Resolver.url("bintray-sbt-plugins", url("https://dl.bintray.com/eed3si9n/sbt-plugins/"))(Resolver.ivyStylePatterns)
resolvers += "scalacache" at "https://mvnrepository.com/artifact/com.github.cb372/scalacache-core"

//Library
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.5"
libraryDependencies += "net.liftweb" %% "lift-json" % "3.2.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "com.github.cb372" %% "scalacache-core" % "0.24.2"
libraryDependencies += "com.github.cb372" %% "scalacache-guava" % "0.24.2"


// Pluggins Config
mainClass := Some("br.ufms.facom.ma.app.LCVQEApp")


