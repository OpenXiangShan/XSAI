/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

import mill._
import scalalib._
import scalafmt._
import $packages._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.common
import $file.XSAICache.common
import $file.CUTE.common

/* for publishVersion */
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import java.io.{BufferedReader, InputStreamReader}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.util.matching.Regex

val defaultScalaVersion = "2.13.15"
val pwd = os.Path(sys.env("MILL_WORKSPACE_ROOT"))

def defaultVersions = Map(
  "chisel"        -> ivy"org.chipsalliance::chisel:6.7.0",
  "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.7.0",
  "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:6.0.0"
)
/* resolve firtool dependency */
import $ivy.`org.chipsalliance::chisel:6.7.0`
import $ivy.`org.chipsalliance::firtool-resolver:1.3.0`

trait HasChisel extends SbtModule {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(defaultVersions("chisel"))

  def chiselPluginIvy: Option[Dep] = Some(defaultVersions("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)

  def resolveFirtoolDeps = T {
    firtoolresolver.Resolve(chisel3.BuildInfo.firtoolVersion.get, true) match {
      case Right(bin) => bin.path.getAbsolutePath
      case Left(err) => err
    }
  }
}

object rocketchip
  extends $file.`rocket-chip`.common.RocketChipModule
    with HasChisel {
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = pwd / "rocket-chip"

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.7"

  object macros extends Macros

  trait Macros
    extends $file.`rocket-chip`.common.MacrosModule
      with SbtModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
  }

  object hardfloat
    extends $file.`rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = pwd / "rocket-chip" / "hardfloat" / "hardfloat"

  }

  object cde
    extends $file.`rocket-chip`.cde.common.CDEModule with ScalaModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = pwd / "rocket-chip" / "cde" / "cde"
  }
}

object utility extends HasChisel {

  override def millSourcePath = pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::sourcecode:0.4.2",
  )

  object test extends SbtTests with TestModule.ScalaTest {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.7")
  }
}

object yunsuan extends HasChisel {

  override def millSourcePath = pwd / "yunsuan"

}

object openNCB extends SbtModule with HasChisel {

  override def millSourcePath = pwd / "XSAICache" / "OpenNCB"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )

}

object zhujiangCompat extends SbtModule with HasChisel {

  override def millSourcePath = pwd / "XSAICache" / "ZhuJiang"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    utility
  )

  override def sources = T.sources {
    val sourceRoots = Seq(
      millSourcePath / "src" / "main" / "scala",
      millSourcePath / "xs-utils" / "src" / "main" / "scala"
    )
    sourceRoots
      .flatMap(os.walk(_))
      .filter(path => os.isFile(path) && path.ext == "scala")
      .filterNot(_.last == "XsStage.scala")
      .map(PathRef(_))
  }

}

object XSAICache extends $file.XSAICache.common.XSCacheModule with HasChisel {

  override def millSourcePath = pwd / "XSAICache"

  def openNCBModule: ScalaModule = openNCB

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  override def moduleDeps = super.moduleDeps ++ Seq(zhujiangCompat)

  override def sources = T.sources {
    super.sources() ++ Seq(
      PathRef(millSourcePath / "src" / "test" / "scala" / "ZhuJiangBridge.scala")
    )
  }

}

object difftest extends HasChisel {

  override def millSourcePath = pwd / "difftest"

  object test extends SbtTests with TestModule.ScalaTest {
    override def sources = T.sources {
      super.sources() ++ Seq(PathRef(this.millSourcePath / "src" / "generator" / "chisel"))
    }
  }

}

object fudian extends HasChisel {

  override def millSourcePath = pwd / "fudian"

}

object chiselAIA extends HasChisel {
  override def millSourcePath = pwd / "ChiselAIA"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    utility
  )
}

object macros extends ScalaModule {

  override def millSourcePath = pwd / "macros"

  override def scalaVersion: T[String] = T(defaultScalaVersion)

  override def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}")

  def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
}

object chiselIOPMP extends HasChisel {
  override def millSourcePath = pwd / "ChiselIOPMP"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    utility
  )
}

object CUTE extends $file.CUTE.common.CUTEModule with HasChisel {

  override def millSourcePath = pwd / "CUTE"

  override def sources = T.sources {
    val cuteSourceRoot = millSourcePath / "src" / "main" / "scala"
    val cuteSources = os.walk(cuteSourceRoot)
      .filter(p => os.isFile(p) && p.ext == "scala")
      .map(PathRef(_))

    val cuteFpeSourceRoot = millSourcePath / "cute-fpe" / "fpe" / "src" / "main" / "scala" / "top"
    val cuteFpeSources = os.list(cuteFpeSourceRoot)
      .filter(p => os.isFile(p) && p.ext == "scala" && p.last != "main.scala")
      .map(PathRef(_))

    cuteSources ++ cuteFpeSources
  }

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def coupledL2Module: ScalaModule = XSAICache
  
  def difftestModule: ScalaModule = difftest

  object test extends SbtTests with TestModule.ScalaTest {
    override def ivyDeps = Agg(
      ivy"edu.berkeley.cs::chiseltest:6.0.0",
      ivy"org.scalatest::scalatest:3.2.17"
    )
    override def moduleDeps = super.moduleDeps ++ Seq(difftest.test)
  }

}

// extends this trait to use XiangShan in other projects
trait XiangShanModule extends ScalaModule {

  def rocketModule: ScalaModule

  def difftestModule: ScalaModule

  def xsaiCacheModule: ScalaModule

  def fudianModule: ScalaModule

  def utilityModule: ScalaModule

  def yunsuanModule: ScalaModule

  def chiselAIAModule: ScalaModule

  def macrosModule: ScalaModule

  def chiselIOPMPModule: ScalaModule

  def cuteModule: ScalaModule

  def zhujiangCompatModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    difftestModule,
    xsaiCacheModule,
    yunsuanModule,
    fudianModule,
    utilityModule,
    chiselAIAModule,
    macrosModule,
    chiselIOPMPModule,
    cuteModule,
    zhujiangCompatModule
  )

  val resourcesPATH = pwd.toString() + "/src/main/resources"
  val envPATH = sys.env("PATH") + ":" + resourcesPATH

  override def forkEnv = Map("PATH" -> envPATH)
}

object xiangshan extends XiangShanModule with HasChisel with ScalafmtModule {

  override def millSourcePath = pwd

  def rocketModule = rocketchip

  def difftestModule = difftest

  def xsaiCacheModule = XSAICache

  def fudianModule = fudian

  def utilityModule = utility

  def yunsuanModule = yunsuan

  def chiselAIAModule = chiselAIA

  def macrosModule = macros

  def chiselIOPMPModule = chiselIOPMP

  def cuteModule = CUTE

  def zhujiangCompatModule = zhujiangCompat

  // properties may be changed by user. Use `Task.Input` here.
  def forkArgsTask = Task.Input {
    Seq(s"-Xmx${sys.props.getOrElse("jvm-xmx", "40G")}", s"-Xss${sys.props.getOrElse("jvm-xss", "256m")}")
  }

  override def forkArgs = forkArgsTask()

  override def ivyDeps = super.ivyDeps() ++ Agg(
    defaultVersions("chiseltest"),
    ivy"io.circe::circe-yaml:1.15.0",
    ivy"io.circe::circe-generic-extras:0.14.4"
  )

  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")

  def publishVersion: T[String] = VcsVersion.vcsState().format(
    revHashDigits = 8,
    dirtyHashDigits = 0,
    commitCountPad = -1,
    countSep = "",
    tagModifier = (tag: String) => "[Rr]elease.*".r.findFirstMatchIn(tag) match {
      case Some(_) => "Kunminghu-Release-" + LocalDateTime.now().format(
                                 DateTimeFormatter.ofPattern("MMM-dd-yyyy").withLocale(new Locale("en")))
      case None => "Kunminghu-dev"
    },
    /* add "username, buildhost, buildtime" for non-release version */
    untaggedSuffix = " (%s@%s) # %s".format(
      System.getProperty("user.name"),
      java.net.InetAddress.getLocalHost().getHostName(),
      LocalDateTime.now().format(DateTimeFormatter.ofPattern("MMM dd hh:mm:ss yyyy").withLocale(new Locale("en")))),
  )

  // gitStatus changes frequently and unpredictably. Use `Task.Input` here.
  def gitStatus: T[String] = Task.Input {
    val gitRevParseBuilder = new ProcessBuilder("git", "rev-parse", "HEAD")
    val gitRevParseProcess = gitRevParseBuilder.start()
    val shaReader = new BufferedReader(new InputStreamReader(gitRevParseProcess.getInputStream))
    val sha = shaReader.readLine()

    val gitStatusBuilder = new ProcessBuilder("git", "status", "-uno", "--porcelain")
    val gitStatusProcess = gitStatusBuilder.start()
    val gitStatusReader = new BufferedReader(new InputStreamReader(gitStatusProcess.getInputStream))
    val status = gitStatusReader.readLine()
    val gitDirty = if (status == null) 0 else 1 

    val str =
      s"""|SHA=$sha
          |dirty=$gitDirty
          |""".stripMargin
    str
  }

  def packDifftestResources(destDir: os.Path): Unit = {
    // package difftest source as resources, only git tracked files were collected
    val difftest_srcs = os.proc("git", "ls-files").call(cwd = pwd / "difftest").out
                          .text().split("\n").filter(_.nonEmpty).toSeq
                          .map(os.RelPath(_))
    difftest_srcs.foreach { f =>
      os.copy(pwd / "difftest" / f, destDir / "difftest-src" / f, createFolders = true)
    }

    // package ready-to-run binary as resources
    val ready_to_run = Seq("riscv64-nemu-interpreter-dual-so",
                           "riscv64-nemu-interpreter-so",
                           "riscv64-spike-so")
    ready_to_run.foreach { f =>
      os.copy(pwd / "ready-to-run" / f, destDir / "ready-to-run" / f, createFolders = true)
    }
  }

  override def resources = T.sources {
    os.write(T.dest / "publishVersion", publishVersion())
    os.write(T.dest / "gitStatus", gitStatus())
    os.write(T.dest / "gitModules", os.proc("git", "submodule", "status").call().out.text())
    packDifftestResources(T.dest)
    super.resources() ++ Seq(PathRef(T.dest))
  }

  object test extends SbtTests with TestModule.ScalaTest {
    override def moduleDeps = super.moduleDeps ++ Seq(
      difftestModule.test
    )

    override def forkArgs = forkArgsTask()

    override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")

    val resourcesPATH = pwd.toString() + "/src/main/resources"
    val envPATH = sys.env("PATH") + ":" + resourcesPATH

    override def forkEnv = Map("PATH" -> envPATH)
  }
}
