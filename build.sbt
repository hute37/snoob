

lazy val commonSettings = Seq(
  organization := "org.sgx",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)



lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "snoob",
    mainClass := Some("Main")
      
  )




scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps"
)




resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"), Resolver.sonatypeRepo("snapshots"),
    "JBoss repository" at "https://repository.jboss.org/nexus/content/repositories/"
)



libraryDependencies ++= {
  val scalazVersion = "7.1.0"
  val akkaVersion = "2.4.1"
  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
    "org.scala-lang.modules" %% "scala-async" % "0.9.5",
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion,
    "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
    "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % Test,
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-persistence" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
    "com.github.dnvriend" %% "akka-persistence-inmemory" % "1.1.6",
    "com.ning" % "async-http-client" % "1.7.19",
    "org.jsoup" % "jsoup" % "1.8.1",
    "io.reactivex" %% "rxscala" % "0.25.0",
    "junit" % "junit" % "4.12"  % Test,
    "org.scalatest" %% "scalatest" % "2.2.4" % Test,
    "org.scalacheck" %% "scalacheck" % "1.12.2" % Test
  )
}

fork in Test := true

parallelExecution := false

initialCommands in console := "import scalaz._, Scalaz._"

