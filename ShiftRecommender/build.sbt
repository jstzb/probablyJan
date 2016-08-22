name := "ShiftRecommender"

version := "1.0"

scalaVersion := "2.11.8"

unmanagedBase := file(sys.env("HOME")) / "etc" / "figaro"

libraryDependencies += "com.quantifind" %% "wisp" % "0.0.4"
libraryDependencies += "org.scalanlp" %% "breeze" % "0.12"
libraryDependencies += "org.scalanlp" %% "breeze-viz" % "0.12"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
