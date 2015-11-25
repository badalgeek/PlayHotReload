package io.badal.playCompiler.view

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

import java.io.{File, FileFilter}

import io.badal.playCompiler.view.TwirlProblem.ProblemException
import io.badal.playCompiler.CompilerSettings
import play.twirl.compiler._
import sbt._
import xsbti.Logger

import scala.io.Codec

object Compiler {

  val templateFormats = Map("html" -> "play.twirl.api.HtmlFormat")
  val templateFilter = ".*scala.*"
  val templateCodec = scala.io.Codec("UTF-8")
  val imports = Seq("models._", "controllers._", "play.api.i18n._", "views.%format%._", "play.api.templates.PlayMagic._")

  def compile(compilerSettings: CompilerSettings, log: Logger): Either[Exception, Seq[File]] = {
    import compilerSettings._
    compile1(Seq(appDir), generatedSource, templateFormats, imports, new SimpleFileFilter(_.getName.matches(templateFilter)),
      HiddenFileFilter, templateCodec, false, log)
  }

  def compile1(
                sourceDirectories: Seq[File],
                targetDirectory: File,
                templateFormats: Map[String, String],
                templateImports: Seq[String],
                includeFilter: FileFilter,
                excludeFilter: FileFilter,
                codec: Codec,
                useOldParser: Boolean,
                log: Logger): Either[Exception, Seq[File]] = {

    try {
      syncGenerated(targetDirectory, codec)
      val templates = collectTemplates(sourceDirectories, templateFormats, includeFilter, excludeFilter)
      for ((template, sourceDirectory, extension, format) <- templates) {
        val imports = formatImports(templateImports, extension)
        TwirlCompiler.compile(template, sourceDirectory, targetDirectory, format, imports, codec, inclusiveDot = false, useOldParser = useOldParser)
      }
      Right(generatedFiles(targetDirectory).map(_.getAbsoluteFile))
    } catch {
      case e:TemplateCompilationError => Left(handleError(log, codec, e))
      case t:Throwable => Left(new RuntimeException(t))
    }
  }

  def generatedFiles(targetDirectory: File): Seq[File] = {
    targetDirectory.listFiles.filter(s => s.getName.endsWith("*.template.scala"))
  }

  def syncGenerated(targetDirectory: File, codec: Codec): Unit = {
    generatedFiles(targetDirectory).map(GeneratedSource(_, codec)).foreach(_.sync)
  }

  def collectTemplates(sourceDirectories: Seq[File], templateFormats: Map[String, String], includeFilter: FileFilter,
                       excludeFilter: FileFilter): Seq[(File, File, String, String)] = {
    sourceDirectories.flatMap(sourceDirectory => {
      recursiveListFilesWithFilter(sourceDirectory, includeFilter).flatMap(file => {
        val ext = file.getName.split('.').last
        if (!excludeFilter.accept(file) && templateFormats.contains(ext))
          Some((file, sourceDirectory, ext, templateFormats(ext)))
        else
          None
      })
    })
  }

  private def recursiveListFilesWithFilter(f: File, filter: FileFilter): Array[File] = {
    val these = f.listFiles
    these.filter(f => f.isFile && filter.accept(f)) ++ these.filter(_.isDirectory).flatMap(recursiveListFilesWithFilter(_, filter))
  }

  def formatImports(templateImports: Seq[String], extension: String): String = {
    templateImports.map("import " + _.replace("%format%", extension)).mkString("\n")
  }

  def handleError(log: Logger, codec: Codec, exception: TemplateCompilationError): ProblemException = {
    exception match {
      case TemplateCompilationError(source, message, line, column) =>
        val exception = TwirlProblem.exception(source, codec, message, line, column)
        val reporter = new LoggerReporter(10, log)
        exception.problems foreach { p => reporter.display(p.position, p.message, p.severity) }
        exception
    }
  }
}
