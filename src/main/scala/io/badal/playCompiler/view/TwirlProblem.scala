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

package io.badal.playCompiler.view

import java.io.File

import play.twirl.compiler.{ GeneratedSource, MaybeGeneratedSource }
import sbt.FeedbackProvidedException
import xsbti.{ CompileFailed, Maybe, Position, Problem, Severity }
import scala.io.Codec

object TwirlProblem {

  def positionMapper(codec: Codec): Position => Option[Position] = position => {
    if(!position.sourceFile().isDefined) None
    MaybeGeneratedSource(position.sourceFile().get(), codec) map {
      generated => TemplatePosition(generated, position)
    }
  }

  def exception(source: File, codec: Codec, message: String, line: Int, column: Int) = {
    val column0 = 0 max (column - 1) // convert to 0-based column
    new ProblemException(TemplateProblem(message, TemplatePosition(source, codec, line, column0)))
  }

  class ProblemException(issues: Problem*) extends CompileFailed with FeedbackProvidedException {
    def arguments(): Array[String] = Array.empty
    def problems(): Array[Problem] = issues.toArray
    override def toString = "Twirl compilation failed"
  }

  case class TemplateProblem(message: String, position: Position) extends Problem {
    def category: String = "undefined"
    def severity: Severity = Severity.Error
  }

  object TemplatePosition {
    def apply(source: File, codec: Codec, line: Int, column: Int): TwirlPosition = {
      val location = TemplateMapping(Some(source), codec).location(line, column)
      new TwirlPosition(Some(source), location)
    }

    def apply(generated: GeneratedSource, position: Position): TwirlPosition = {
      val offset = option(position.offset) map { o => generated.mapPosition(o) }
      val location = offset flatMap { o => TemplateMapping(generated.source, generated.codec).location(o) }
      new TwirlPosition(generated.source, location)
    }
  }

  class TwirlPosition(source: Option[File], location: Option[TemplateMapping.Location]) extends Position {
    val line: Maybe[Integer] = maybe { location map (_.line) }

    val lineContent: String = location.fold("")(_.content)

    val offset: Maybe[Integer] = maybe { location map (_.offset) }

    val pointer: Maybe[Integer] = maybe { location map (_.column) }

    val pointerSpace: Maybe[String] = maybe {
      location.map { l => lineContent.take(l.column) map { case '\t' => '\t'; case _ => ' ' } }
    }

    val sourceFile: Maybe[File] = maybe(source)

    val sourcePath: Maybe[String] = maybe { source map (_.getCanonicalPath) }
  }

  object TemplateMapping {
    case class Location(line: Int, column: Int, offset: Int, content: String)

    case class Line(line: Int, start: Int, end: Int, content: String) {
      def location(l: Int, c: Int): Location = {
        if (l < line) {
          Location(line, 0, start, content)
        } else if (l > line) {
          Location(line, content.length, end, content)
        } else {
          val column = 0 max c min content.length
          val offset = start + column
          Location(line, column, offset, content)
        }
      }

      def location(o: Int): Location = {
        val offset = start max o min end
        val column = offset - start
        Location(line, column, offset, content)
      }
    }

    def apply(source: Option[File], codec: Codec): TemplateMapping = {
      val lines = source map { file => sbt.IO.readLines(file, codec.charSet) }
      TemplateMapping(lines.get)
    }
  }

  case class TemplateMapping(sourceLines: Seq[String]) {
    import TemplateMapping.{ Line, Location }

    val lines: Seq[Line] = sourceLines.scanLeft(Line(0, -1, -1, "")) {
      (previous, content) => Line(previous.line + 1, previous.end + 1, previous.end + 1 + content.length, content)
    }.drop(1)

    def location(offset: Int): Option[Location] = {
      if (lines.isEmpty) {
        None
      } else {
        val index = 0 max lines.lastIndexWhere(_.start <= offset)
        Some(lines(index).location(offset))
      }
    }

    def location(line: Int, column: Int): Option[Location] = {
      if (lines.isEmpty) {
        None
      } else {
        val index = 0 max (line - 1) min (lines.length - 1)
        Some(lines(index).location(line, column))
      }
    }
  }

  def maybe[A](o: Option[A]): Maybe[A] = o match {
    case Some(v) => Maybe.just(v)
    case None => Maybe.nothing()
  }

  def option[A](o: Maybe[A]): Option[A] = {
    if(o.isEmpty)
      None
    else
      Some(o.get)
  }
}
