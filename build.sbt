import sbtassembly.AssemblyPlugin.autoImport._

ThisBuild / scalaVersion := "2.13.16" // or whatever version you're using


libraryDependencies ++= Seq(
  "com.badlogicgames.gdx" % "gdx" % "1.12.1",
  "com.badlogicgames.gdx" % "gdx-backend-lwjgl3" % "1.12.1",
  "com.badlogicgames.gdx" % "gdx-platform" % "1.12.1" classifier "natives-desktop"
)

lazy val root = (project in file("."))
  .settings(
    name := "ElevenCell3D",
    version := "0.1.0",
    assembly / mainClass := Some("Main"), // replace with your main class
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  )

  



