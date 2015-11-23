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

import io.badal.playCompiler.router.{Compiler => RouteCompiler}
import io.badal.playCompiler.source.{Compiler => SourceCompiler}
import io.badal.playCompiler.view.TwirlProblem.ProblemException
import io.badal.playCompiler.view.{Compiler => ViewCompiler}
import play.api.{PlayException, UnexpectedException}
import play.routes.compiler.RoutesCompilationError
import play.runsupport.Reloader._
import sbt.{Logger, IO}
import xsbti.{Problem, Severity}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by sbadal on 11/23/15.
  */
object Compiler {

  def reloadCompiler: (CompilerSettings, Logger) =>
    () => CompileResult = (compilerSetting: CompilerSettings, log: Logger) => {

    import compilerSetting._
    val compiler = SourceCompiler.getCompiler(compilerSetting, log)
    val routeCompiler = RouteCompiler.getCompiler(compilerSetting, log)

    () =>
      ViewCompiler.compile(compilerSetting, log).left.map(
        _ match {
          case ex: ProblemException => CompileFailure(new CompilationException(ex.problems().filter(_.severity() == Severity.Error)(0)))
          case e: Exception => CompileFailure(new UnexpectedException(None, Some(e)))
          case _ => CompileFailure(new UnexpectedException(None, None))
        }
      ).right.map(
        _ =>
          routeCompiler.compile().left.map(
            x => CompileFailure(new RouterCompilationException(x.headOption.get))
          ).right.map(
            x => compiler.compile(appDir, generatedSource).left.map(
              _ match {
                case ex: PlayException => CompileFailure(ex)
                case e: xsbti.CompileFailed => CompileFailure(new CompilationException(e.problems().filter(_.severity() == Severity.Error)(0)))
                case e: Exception => CompileFailure(new UnexpectedException(None, Some(e)))
                case _ => CompileFailure(new UnexpectedException(None, None))
              }).right.map(
              analysis => CompileSuccess(sourceMap(analysis), Seq(compileDir))
            ).fold(identity, identity)
          ).fold(identity, identity)
      ).fold(identity, identity)
  }

  def sourceMap(analysis: sbt.inc.Analysis): SourceMap = {
    analysis.apis.internal.foldLeft(Map.empty[String, Source]) {
      case (sourceMap, (file, source)) => sourceMap ++ {
        source.api.definitions map { d => d.name -> Source(file, originalSource(file)) }
      }
    }
  }

  def originalSource(file: File): Option[File] = {
    play.twirl.compiler.MaybeGeneratedSource.unapply(file).map(_.file)
  }

}

class RouterCompilationException(error: RoutesCompilationError)
  extends PlayException.ExceptionSource("Route Compilation error", error.message) {
  def input(): String = IO.read(error.source)

  def line(): Integer = error.line.get

  def position(): Integer = error.column.get

  def sourceName(): String = error.source.getName()
}

class CompilationException(problem: Problem)
  extends PlayException.ExceptionSource("Compilation error", problem.message()) {
  def input(): String = IO.read(problem.position.sourceFile.get())

  def line(): Integer = problem.position().line().get()

  def position(): Integer = problem.position().pointer().get()

  def sourceName(): String = problem.position().sourcePath().get()
}