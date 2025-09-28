import sbtassembly.AssemblyPlugin.autoImport._

ThisBuild / scalaVersion := "2.13.16" 


libraryDependencies ++= Seq(
  "com.badlogicgames.gdx" % "gdx" % "1.12.1",
  "com.badlogicgames.gdx" % "gdx-backend-lwjgl3" % "1.12.1",
  "com.badlogicgames.gdx" % "gdx-platform" % "1.12.1" classifier "natives-desktop"
)

lazy val root = (project in file("."))
  .settings(
    name := "ElevenCell3D",
    version := "0.1.0",
    assembly / mainClass := Some("Launcher"), 
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case "module-info.class"           => MergeStrategy.discard

      // Keep all native files
      case x if x.endsWith(".dll") || x.endsWith(".so") || x.endsWith(".dylib") =>
        MergeStrategy.first

      case _ => MergeStrategy.first
    }
  )

  



