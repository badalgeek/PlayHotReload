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
import sbt.{ConsoleLogger, Level}

/**
  * Created by badal on 9/24/15.
  */
class ServerRunner {


  def runInturrupted(compilerSetting: CompilerSettings) = {
    run(compilerSetting)
  }

  def runWithCloseHook(compilerSetting: CompilerSettings) = {
    var mainThread = Thread.currentThread()
    var devModeServer = run(compilerSetting)

    println()
    println("(Server started, use Ctrl+C to stop and go back to the console...)")
    println()
    OpenApp.open("http://localhost:8080")
    sys.addShutdownHook(devModeServer.close())

    while(true) {
      Thread.sleep(10000)
    }
  }


  def run(compilerSetting: CompilerSettings): PlayDevServer = {

    import compilerSetting._
    createCompileIfNotExist(compileDir)

    val dependency = System.getProperty("java.class.path").split(":")
    val externalDep = dependency.filter(_.endsWith("jar"))

    val runHooks = Seq.empty[RunHook]
    val javaOptions = Seq.empty[String]
    val dependencyClassPath = externalDep.map(new File(_))
    val dependencyURLClassLoader = Reloader.createURLClassLoader
    val log = logger(Level.Info, true)

    val reloadCompile = Compiler.reloadCompiler(compilerSetting, log)
    val reloadClassLoader = Reloader.createDelegatedResourcesClassLoader
    val parent = ClassLoader.getSystemClassLoader.getParent
    val commonClassLoader = new URLClassLoader(Array[URL](), parent)
    val monitoredFile = Seq(confDirectory, appDir)

    val fileWatchService = FileWatchService.sbt(1000)
    val docsClassPath = dependency.filter(_.endsWith("jar")).map(new File(_))
    val playDoc = dependency.filter(_.contains("play-omnidoc")).map(new File(_)).headOption
    val assetClassLoader = (parent: ClassLoader) => {
      new AssetsClassLoader(parent, Seq(("/public", new File("target/testApp/public"))))
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
      8080,
      "localhost",
      new File("build/testResource"),
      Seq.empty[(String, String)],
      Seq.empty[String],
      null,
      "play.core.server.DevServerStart"
    )
  }


  private def createCompileIfNotExist(compileDir: File) = {
    if (!compileDir.exists()) {
      compileDir.mkdir()
    }
  }

  private def logger(level: Level.Value, color: Boolean): sbt.Logger = {
    val log = ConsoleLogger(useColor = ConsoleLogger.formatEnabled && color)
    log.setLevel(level)
    log
  }
}



