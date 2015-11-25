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

package io.badal.playcompiler
import java.io.{PrintWriter, File}

import io.badal.playCompiler.{ServerSettings, CompilerSettings, ServerRunner}
import org.apache.commons.io.FileUtils
import org.junit.{Before, Test}
import org.scalatest.junit.AssertionsForJUnit
import sbt.ConsoleLogger

import scala.collection.mutable.ListBuffer


/**
  * Created by sbadal on 9/28/15.
  */
class ServerRunnerTest extends AssertionsForJUnit {

  @Before def initialize() {
  }


  @Test def verifyEasy() {
    // Uses JUnit-style assertions
    val confDirectory = new File("target/testApp/conf")
    val generatedDir = new File("target/testApp/target/generated-sources")
    val compiledDir = new File("target/testApp/target/classes")
    val compileCacheDir: File = new File("target/testApp/target/compile-cache")
    if (!compiledDir.exists()) {
      compiledDir.mkdir()
    }
    if (!generatedDir.exists()) {
      generatedDir.mkdir()
    }
    FileUtils.copyFile(new File("target/testApp/conf/application.conf"), new File("target/testApp/target/classes/application.conf"))

    val fileList = new ListBuffer[String]
    val consoleOut: ConsoleLogger = ConsoleLogger.apply(new PrintWriter(System.out))
    val assetMap: Map[String, File] = Map(("public", new File("target/testApp/public")))
    ServerRunner.runWithCloseHook(
      new CompilerSettings(new File("target/testApp/app"), compiledDir, compileCacheDir,
        confDirectory, new File("target/testApp/target/generated-sources"), System.getProperty("java.class.path"), "2.10"),
      new ServerSettings("localhost", 8081, new File("target/testApp/target/log"), assetMap),
      consoleOut
    )
  }

}
