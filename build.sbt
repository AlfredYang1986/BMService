name := "BMService"

version := "3.0"

libraryDependencies ++= Seq(
	jdbc,
	anorm,
	cache
	// "org.glassfish.hk2" %% "hk2-utils" % "2.22.2",
	// "org.glassfish.hk2" %% "hk2-locator" % "2.22.2"
	// "org.apache.spark" %% "spark-core" % "2.1.0",
	// "com.typesafe.akka" %% "akka-actor" % "2.4.16",
	// "com.typesafe.akka" %% "akka-slf4j" % "2.2.3"
)     

play.Project.playScalaSettings