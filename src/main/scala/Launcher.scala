import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters._

object Launcher {
  def main(args: Array[String]): Unit = {

    println("".split(",").toList)
    val osName = System.getProperty("os.name").toLowerCase
    val inputArgs = ManagementFactory.getRuntimeMXBean.getInputArguments.toString

    if (osName.contains("mac") && !inputArgs.contains("-XstartOnFirstThread")) {
      // Relaunch JVM with -XstartOnFirstThread
      val javaBin = System.getProperty("java.home") + "/bin/java"
      val classpath = System.getProperty("java.class.path")
      val mainClass = "Main" // <-- replace with your actual main class

      val cmd = Seq(
        javaBin,
        "-XstartOnFirstThread"
      ) ++
        ManagementFactory.getRuntimeMXBean.getInputArguments.asScala ++
        Seq("-cp", classpath, mainClass) ++ args

    //   println(s"Relaunching JVM with -XstartOnFirstThread: ${cmd.mkString(" ")}")
      val pb = new ProcessBuilder(cmd: _*)
      pb.inheritIO()
      pb.start()
      sys.exit(0)
    }

    // Otherwise, just start normally
    Main.main(args)
  }
}