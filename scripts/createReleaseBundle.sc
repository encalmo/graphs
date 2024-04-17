#!/usr/bin/env -S scala-cli shebang --quiet

//> using scala 3.4.1
//> using jvm 21
//> using toolkit default
import java.nio.file.Path

import scala.io.AnsiColor.*

val organization = getArg("organization")
val name = getArg("name")
val version = getArg("version")
val secretKeyPassword = getArg("secret-key-password")
val maybeGpgKey = maybeArg("gpg-key")
val maybeSecretKey = maybeArg("secret-key")

val signer = maybeGpgKey match {
  case Some(key) => s"""--signer gpg --gpg-key $key"""
  case None =>
    maybeSecretKey match {
      case Some(key) => s"""--signer bc --secret-key $key"""
      case None      => ""
    }

}

println(s"${GREEN}running tests ...${RESET}")

call(s"scala-cli test .").foreach(println)

println(s"${GREEN}publishing package locally ...${RESET}")

val command =
  s"""scala-cli publish local . --organization $organization --name $name --project-version $version --secret-key-password value:${secretKeyPassword} $signer"""

val (publishedFolder, coordinates) = {
  val ivyLocation = call(command).last.trim()
  val coordinates = {
    ivyLocation.drop(ivyLocation.indexOf(organization)).split("/")
  }
  (
    if (ivyLocation.startsWith("~" + java.io.File.separator))
    then os.Path(java.io.File(System.getProperty("user.home") + ivyLocation.substring(1)))
    else os.Path(java.io.File(ivyLocation).getAbsoluteFile()),
    coordinates
  )
}

val artefactName = coordinates.dropRight(1).last

println(s"${GREEN}published ${coordinates.mkString(":")} to $publishedFolder${RESET}")

val tempDir = os.temp.dir(prefix = s"sonatype-deployment-package-")
val bundleFolderPath = tempDir / toFolderPath(coordinates)
os.makeDir.all(bundleFolderPath)

println(s"${GREEN}preparing sonatype bundle in ${bundleFolderPath} ...${RESET}")

copyPublishedFiles("poms")
copyPublishedFiles("jars")
copyPublishedFiles("srcs")
copyPublishedFiles("docs")

println(s"${GREEN}creating bundle archive ...${RESET}")
val bundleArchivePath = tempDir / "bundle.zip"
call(s"zip -r bundle.zip ${bundleFolderPath.relativeTo(tempDir)}", cwd = tempDir).foreach(println)
call(s"ls -l $bundleArchivePath").foreach(println)

// ---------- UTILS ----------

def call(command: String, cwd: os.Path = os.pwd): Seq[String] =
  println(s"${BLUE}command: ${command}${RESET}")
  val commandArray = command.split(" ")
  val commandResult = os.proc(commandArray).call(check = false, cwd = cwd)
  if (commandResult.exitCode != 0)
  then {
    println(
      s"${WHITE}${RED_B}[FATAL] script's command ${YELLOW}${commandArray(0)}${WHITE} returned ${commandResult.exitCode} ${RESET}"
    )
    System.exit(2)
    Seq.empty
  } else commandResult.out.lines()

def toFolderPath(coordinates: Array[String]): os.RelPath =
  os.RelPath(coordinates(0).split("\\.") ++ coordinates.drop(1), 0)

def copyPublishedFiles(folder: String) = {
  os.list(publishedFolder / folder)
    .foreach { path =>
      val targetName = path.last.replace(artefactName, s"$artefactName-$version")
      println(s"${GREEN}- copying $targetName ...${RESET}")
      os.copy(from = path, to = bundleFolderPath / targetName)
    }
}

def getArg(key: String): String =
  maybeArg(key).getOrElse {
    println(s"${WHITE}${RED_B}Missing parameter $key${RESET}")
    System.exit(2)
    ""
  }

def maybeArg(key: String): Option[String] =
  val prefix = s"--$key="
  args
    .filter(_.startsWith(prefix))
    .headOption
    .map(_.drop(prefix.length()))
