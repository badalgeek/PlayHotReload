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

import scala.collection.mutable.ListBuffer

/**
  * Created by badal on 11/17/15.
  */
object FileUtil {

  def filesInDirEndingWith(dir: File, end: String): Seq[File] = {
    val files = dir.listFiles
    if (files == null) throw new RuntimeException("Directory " + dir.getAbsolutePath + " not found")

    val filesInDir = files.filter(_.isFile).filter(_.getName.endsWith(end))

    // recurse into subdirectories
    var nestedFiles : ListBuffer[File] = new ListBuffer[File]
    files.filter(_.isDirectory).foreach { dir => nestedFiles ++= filesInDirEndingWith(dir, end) }

    filesInDir ++ nestedFiles
  }

  def filesInDirStartingWith(dir: File, start: String): Seq[File] = {
    val files = dir.listFiles
    if (files == null) throw new RuntimeException("Directory " + dir.getAbsolutePath + " not found")

    val filesInDir = files.filter(_.isFile).filter(_.getName.startsWith(start))

    // recurse into subdirectories
    var nestedFiles : ListBuffer[File] = new ListBuffer[File]
    files.filter(_.isDirectory).foreach { dir => nestedFiles ++= filesInDirStartingWith(dir, start) }

    filesInDir ++ nestedFiles
  }
}
