name := "ticketprob"

scalaVersion := "2.9.2"

version := "1.0"

resolvers ++= Seq(
  "sonetype" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "cc.spray" at "http://repo.spray.io/"
)

libraryDependencies ++= List(
  "org.scalala" % "scalala_2.9.0" % "1.0.0.RC2-SNAPSHOT",
  "cc.spray"   %% "spray-json"       % "1.1.1",
  "com.github.tototoshi" %% "scala-csv" % "1.0.0"
)
