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

package io.badal.playCompiler.source

import java.io.File

import io.badal.playCompiler.CompilerSettings
import sbt.Path._
import sbt.compiler._
import sbt.compiler.javac.{IncrementalCompilerJavaTools, JavaTools}
import sbt.inc.{Analysis, FileBasedStore, IncOptions, Locate}
import sbt.{ScalaInstance, _}
import xsbti.compile.{CompileOrder, GlobalsCache}
import xsbti.{AppProvider, ApplicationID, Launcher, Logger, ScalaProvider}

import scala.collection._

/**
  * Created by sbadal on 11/6/15.
  */
object Compiler {

  def getCompiler(compilerSettings: CompilerSettings, log: Logger): Compiler = {
    import compilerSettings._

    val jars = System.getProperty("java.class.path").split(System.getProperty("path.separator"))
    val scalaInstance: ScalaInstance = createScalaInstance(scalaVersion, jars)

    val interfaceJar: File = getInterfaceJar(compileCacheDir, scalaVersion, jars, scalaInstance, log)
    val scalac = IC.newScalaCompiler(scalaInstance, interfaceJar, ClasspathOptions.boot)
    val javaToolCompiler = JavaTools.directOrFork(scalaInstance, ClasspathOptions.boot, Some(getJavaHomeForHostVm))

    new Compiler(scalac, javaToolCompiler, scalaInstance.actualVersion, log, compileDir, jars.map(new File(_)),
      new File(compileCacheDir.getAbsolutePath + "/SourceAnalysisCache"))
  }

  private def getJavaHomeForHostVm: File = {
    var javaHome: File = new File(System.getProperty("java.home"))
    val hasJavacInBin: (File) => Boolean = file =>
      new File(file.getPath + "/bin/").listFiles().count(_.getName.equals("javac")) != 1
    if (hasJavacInBin(javaHome)) {
      javaHome = javaHome.getParentFile
      if (hasJavacInBin(javaHome)) {
        throw new RuntimeException("JDK not found. Probably you are using JRE to run this incremental compiler")
      }
    }
    javaHome
  }

  private def getInterfaceJar(compileCacheDir: File, scalaVersion: String, jars: Array[String],
                              scalaInstance: ScalaInstance, log: Logger): File = {
    val dir = new RichFile(compileCacheDir) / interfaceId(scalaInstance.actualVersion)
    val interfaceJar = new RichFile(dir) / ("compiler-interface" + ".jar")
    val sbtInterfaceJar = getJarFile("sbt-interface", jars)
    val compilerInterfaceJar = getJarFile("compiler-interface", jars)
    if (!interfaceJar.exists) {
      dir.mkdirs()
      IC.compileInterfaceJar(interfaceId(scalaVersion), compilerInterfaceJar, interfaceJar, sbtInterfaceJar, scalaInstance, log)
    }
    interfaceJar
  }

  private def getJarFile: (String, Seq[String]) => File = (name,  jars) => {
    val jar = jars.find(s => s.contains(name) && s.endsWith("jar"))
    jar match {
      case Some(value) => new File(value)
      case None => throw new NullPointerException("Could not find jar with name " + name)
    }
  }

  private def createScalaInstance(scalaVersion: String, jars: Array[String]): ScalaInstance = {
    val scalaCompilerJar = getJarFile("scala-compiler", jars)
    val scalaLibraryJar = getJarFile("scala-library", jars)
    val scalaReflectJar = getJarFile("scala-reflect", jars)
    val scalaExtraJar = Array(scalaReflectJar)

    val scalaInstance = ScalaInstance.apply(scalaVersion, new ScalaProvider {
      override def launcher(): Launcher = null
      override def app(id: ApplicationID): AppProvider = null
      override def compilerJar(): File = scalaCompilerJar
      override def loader(): ClassLoader = classOf[Compiler].getClassLoader
      override def libraryJar(): File = scalaLibraryJar
      override def version(): String = scalaVersion
      override def jars(): Array[File] = scalaExtraJar
    })
    scalaInstance
  }

  private def interfaceId(scalaVersion: String) =
    "compiler-interface-" + scalaVersion + "-" + System.getProperty("java.class.version")

  private def logger(level: Level.Value, color: Boolean): Logger = {
    val log = ConsoleLogger(useColor = ConsoleLogger.formatEnabled && color)
    log.setLevel(level)
    log
  }

}

class Compiler(scalac: AnalyzingCompiler, javaToolCompiler: IncrementalCompilerJavaTools, version: String, log: Logger,
               val compileDir: File, classpathJars: Array[File], compileCacheFile: File) {

  var fileBasedStore = FileBasedStore.apply(compileCacheFile)
  var compileClasspath = autoClasspath(compileDir, scalac.scalaInstance.allJars, classpathJars)
  var globalsCache: GlobalsCache = createResidentCache(1000)
  var compileSetUp: Option[CompileSetup] = None
  var previousAnalysis: Analysis = IC.readAnalysis(compileCacheFile, IncOptions.Default)
  var getAnalysis: File => Option[Analysis] = initializeAnalysisForDependentLibraries

  private def normalise: File => File = {
    _.getAbsoluteFile
  }

  private def normaliseAll: (Array[File]) => Array[File] = {
    _.map(f => normalise(f))
  }

  private def recursiveListFilesWithFilter(f: File, filter: String): Array[File] = {
    val these = f.listFiles
    these.filter(f => f.isFile && f.name.matches(filter)) ++ these.filter(_.isDirectory).flatMap(recursiveListFilesWithFilter(_,filter))
  }

  def compile(appDir: File, generatedSource: File): Either[Exception, Analysis] = {
    compile(normaliseAll(recursiveListFilesWithFilter(appDir, ".*(java|scala)")) ++ normaliseAll(recursiveListFilesWithFilter(generatedSource,".*(java|scala)")))
  }

  def compile(sources: Seq[File]): Either[Exception, Analysis] = {
    try {
      var loggerReporter = new LoggerReporter(100, log, identity)
      val result = IC.incrementalCompile(scalac, javaToolCompiler.xsbtiCompiler, sources, compileClasspath,
        CompileOutput(compileDir), globalsCache, None, Nil, Nil, previousAnalysis, compileSetUp, getAnalysis,
        Locate.definesClass _, loggerReporter, CompileOrder.Mixed, false, IncOptions.Default)(log)
      previousAnalysis = result.analysis
      compileSetUp = Some(result.setup)
      fileBasedStore.set(previousAnalysis, result.setup)
      Right(result.analysis)
    } catch {
      case ex: Exception =>
        Left(ex)
    }

  }

  private def initializeAnalysisForDependentLibraries: File => Option[Analysis] = {
    val upstreamAnalysis = Map.empty[File, File] map { case (k, v) => (normalise(k), normalise(v)) }
    val analysisMap = (compileClasspath map {
      file => (file, analysisFor(file, normalise(compileDir),
        upstreamAnalysis))
    }).toMap
    analysisMap.get _
  }

  /**
    * Analysis map for dependent libraries, will try to find if classpath has dependent classes as a directory
    */
  private def analysisFor(file: File, exclude: File, mapped: Map[File, File]): Analysis = {
    cacheFor(file, exclude, mapped) map analysis getOrElse Analysis.Empty
  }

  private def cacheFor(file: File, exclude: File, mapped: Map[File, File]): Option[File] = {
    mapped.get(file) orElse {
      if (file.isDirectory && file != exclude) Some(defaultCacheLocation(file)) else None
    }
  }

  private def analysis(file: File): Analysis = {
    FileBasedStore.apply(file).get map (_._1) getOrElse Analysis.Empty
  }

  private def defaultCacheLocation(classesDir: File) = {
    classesDir.getParentFile / "cache" / classesDir.getName
  }

  private def createResidentCache(maxCompilers: Int): GlobalsCache = {
    if (maxCompilers <= 0) CompilerCache.fresh else CompilerCache(maxCompilers)
  }

  private def autoClasspath(classesDirectory: File, allScalaJars: Seq[File], classpath: Seq[File]): Seq[File] = {
    classesDirectory +: classpath
  }

}
