val scala3Version = "3.8.3"

// TODO: Upgrading this to 3.4.1 causes a segfault?
val lwjglVersion = "3.3.3"
val isOsx = System.getProperty("os.name").toLowerCase().contains("osx")
val lwjglNatives = {
  val name = System.getProperty("os.name", "unknown").toLowerCase()
  if (name.contains("windows")) {
    val arch = System.getProperty("os.arch")
    if (arch.contains("64")) {
      s"natives-windows${if (arch.startsWith("aarch64")) "-arm64" else ""}"
    } else {
      "natives-windows-x86"
    }
  } else if (name.contains("osx")) {
    if (System.getProperty("os.arch").startsWith("aarch64")) "natives-macos-arm64" else "natives-macos"
  } else if (name.contains("linux") || name.contains("unix")) {
    val arch = System.getProperty("os.arch")
    val suffix = 
      if (arch.startsWith("arm") || arch.startsWith("aarch64")) {
        if (arch.contains("64") || arch.startsWith("armv8")) "-arm64" else "-arm32"
      } else if (arch.startsWith("ppc")) {
        "-ppc64le"
      } else if (arch.startsWith("riscv")) {
        "-riscv64"
      } else {
        ""
      }
    s"natives-linux${suffix}"
  } else {
  // why
    ""
  }
}

lazy val root = project
  .in(file("."))
  .settings(
    name := "personagame",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "lwjgl",
      "lwjgl-assimp",
      "lwjgl-glfw",
      "lwjgl-openal",
      "lwjgl-stb",
      "lwjgl-opengl",
      "lwjgl-bgfx",
      "lwjgl-freetype"
    ).map("org.lwjgl" % _ % lwjglVersion),
    libraryDependencies += "org.joml" % "joml" % "1.10.8",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.4.3",
    libraryDependencies ++= Seq(
      "lwjgl",
      "lwjgl-assimp",
      "lwjgl-glfw",
      "lwjgl-openal",
      "lwjgl-stb",
      "lwjgl-opengl",
      "lwjgl-bgfx",
      "lwjgl-freetype"
    ).map("org.lwjgl" % _ % lwjglVersion % Runtime classifier lwjglNatives),
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",
    Compile / run / fork := true,

  )
