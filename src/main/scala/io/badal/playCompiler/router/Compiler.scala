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

package io.badal.playCompiler.router

import java.io._

import io.badal.playCompiler.CompilerSettings
import play.routes.compiler._
import sbt.Logger

object Compiler {

  def getCompiler(compilerSettings: CompilerSettings, log: Logger): Compiler = {
    import compilerSettings._
    new Compiler(new File(compileCacheDir.getAbsolutePath + "/RouterOp"), confDirectory, generatedSource,
      Seq.empty, true, log)
  }

  def getCompiler(compilerSettings: CompilerSettings, additionalImports: Seq[String],
                  generateReverseRouter: Boolean, log: Logger): Compiler = {
    import compilerSettings._
    new Compiler(new File(compileCacheDir.getAbsolutePath + "/RouterOp"), confDirectory, generatedSource,
      additionalImports, generateReverseRouter, log)
  }
}

class Compiler(compileCacheFile: File, confDirectory: File, generatedDir: File, additionalImports: Seq[String],
               generateReverseRouter: Boolean, log: Logger) {

  var routeManager = new RouterInfoManager(compileCacheFile)
  var lastRouteInfo: Option[RouterInfo] = routeManager.readRouterInfo

  def compile(): Either[scala.Seq[play.routes.compiler.RoutesCompilationError], scala.Seq[java.io.File]] = {
    if (!confDirectory.exists) return Left(getRouteCompilationError(confDirectory,"Configuration directory doesn't exist"))
    val files = confDirectory.listFiles.filter(_.getName.endsWith("routes"))
    if (files.length > 1) return Left(getRouteCompilationError(confDirectory,"More than one route not yet supported"))
    if (files.length == 0) return Left(getRouteCompilationError(confDirectory,"No route found"))
    val newRouteInfo: RouterInfo = routeManager.getRouteInfo(files(0))
    lastRouteInfo match {
      case Some(routeInfo) =>
        if (newRouteInfo.equals(routeInfo)) {
          log.info("No change in router file.")
          return Right(Seq())
        }
        else if (newRouteInfo.routerFile.equals(routeInfo.routerFile) && newRouteInfo.lastModified == routeInfo.lastModified){
          log.info("Last router compile resulted in error. But file is not yet touched.")
          return Left(routeInfo.errors)
        }
      case _ =>
    }
    compileFile(files(0)) match {
      case Left(x) =>
        newRouteInfo.errors = x
        lastRouteInfo = Some(newRouteInfo)
        routeManager.writeOptionInfo(newRouteInfo)
        log.error("Router compilation ended with error.")
        return Left(x)
      case Right(y) =>
        lastRouteInfo = Some(newRouteInfo)
        routeManager.writeOptionInfo(newRouteInfo)
        log.info("Route compilation succeeded.")
        return Right(y)
    }
  }

  private def getRouteCompilationError(file: File, message: String): Seq[RoutesCompilationError] = {
    Seq(new RoutesCompilationError(file, message, None, None))
  }

  private def compileFile(file: File): Either[scala.Seq[play.routes.compiler.RoutesCompilationError], scala.Seq[java.io.File]] = {
    val routerTask = new RoutesCompiler.RoutesCompilerTask(file, additionalImports, true, generateReverseRouter, false)
    //Only limiting to one router file for now
    RoutesCompiler.compile(routerTask, StaticRoutesGenerator, generatedDir)
  }

}

@SerialVersionUID(15L)
class RouterInfo(var routerFile: File, var lastModified: Long) extends Serializable {
  var didCompilationSucceed: Boolean = true
  var errors: Seq[play.routes.compiler.RoutesCompilationError] = Seq.empty

  override def equals(that: Any): Boolean =
    that match {
      case that: RouterInfo => this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + routerFile.hashCode
    result = prime * result + lastModified.hashCode
    result = prime * result + (if (didCompilationSucceed) 0 else 1)
    result
  }

}

class RouterInfoManager(cacheFile: File) {

  def getRouteInfo(routerFile: File): RouterInfo = {
    new RouterInfo(routerFile, routerFile.lastModified())
  }

  def writeOptionInfo(routerInfo: RouterInfo) = {
    val fos = new FileOutputStream(cacheFile)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(routerInfo)
    oos.close()
  }

  def readRouterInfo(): Option[RouterInfo] = {
    if (!cacheFile.exists()) return None
    val fis = new FileInputStream(cacheFile)
    val ois = new ObjectInputStream(fis)
    val objectVal = ois.readObject()
    ois.close()
    objectVal match {
      case i: RouterInfo =>
        Some(objectVal.asInstanceOf[RouterInfo])
      case _ =>
        None
    }

  }
}