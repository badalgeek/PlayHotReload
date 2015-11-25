/*
 * Copyright 2002-2015 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.badal.playCompiler

import java.io.File
import java.net.{URL, URLClassLoader}

import play.runsupport.Reloader.PlayDevServer
import play.runsupport._
import sbt.{ConsoleLogger, Logger, Level}

/**
  * Created by badal on 9/24/15.
  */
object ServerRunner {

  def runInturrupted(compilerSetting: CompilerSettings, serverSettings: ServerSettings, log:Logger) = {
    run(compilerSetting, serverSettings, log)
  }

  def runWithCloseHook(compilerSetting: CompilerSettings, serverSettings: ServerSettings, log:Logger) = {
    var mainThread = Thread.currentThread()
    var devModeServer = run(compilerSetting, serverSettings, log)

    println()
    println("(Server started, use Ctrl+C to stop and go back to the console...)")
    println()
    OpenApp.open("http://" + serverSettings.hostName + ":" + serverSettings.port)
    sys.addShutdownHook(devModeServer.close())

    while(true) {
      Thread.sleep(10000)
    }
  }

  def run(compilerSetting: CompilerSettings, serverSettings: ServerSettings, log: Logger): PlayDevServer = {

    import compilerSetting._
    import serverSettings._

    val dependency = classPath.split(":")

    log.info("Using classpath to compile scala and java source:")
    dependency.foreach(log.info(_))

    val externalDep = dependency.filter(_.endsWith("jar"))

    val runHooks = Seq.empty[RunHook]
    val javaOptions = Seq.empty[String]
    val dependencyClassPath = externalDep.map(new File(_))
    val dependencyURLClassLoader = Reloader.createURLClassLoader

    val reloadCompile = Compiler.reloadCompiler(compilerSetting, log)
    val reloadClassLoader = Reloader.createDelegatedResourcesClassLoader
    val parent = ClassLoader.getSystemClassLoader.getParent
    val commonClassLoader = new URLClassLoader(Array[URL](), parent)
    val monitoredFile = Seq(confDirectory, appDir)

    val fileWatchService = FileWatchService.sbt(1000)
    val docsClassPath = dependency.filter(_.endsWith("jar")).map(new File(_))
    val playDoc = dependency.filter(_.contains("play-omnidoc")).map(new File(_)).headOption
    val assetTuple = assetMap.map(e => (e._1,e._2)).toSeq
    val assetClassLoader = (parent: ClassLoader) => {
      new AssetsClassLoader(parent, assetTuple)
    }
    Reloader.startDevMode(
      runHooks,
      javaOptions,
      dependencyClassPath,
      dependencyURLClassLoader,
      reloadCompile,
      reloadClassLoader,
      assetClassLoader,
      commonClassLoader,
      monitoredFile,
      fileWatchService,
      docsClassPath,
      playDoc,
      Integer2int(port),
      hostName,
      logDir,
      Seq.empty[(String, String)],
      Seq.empty[String],
      null,
      "play.core.server.DevServerStart"
    )
  }

  private def logger(level: Level.Value, color: Boolean): sbt.Logger = {
    val log = ConsoleLogger(useColor = ConsoleLogger.formatEnabled && color)
    log.setLevel(level)
    log
  }
}



