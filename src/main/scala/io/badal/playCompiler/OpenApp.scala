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

/**
  * Created by sbadal on 11/24/15.
  */
object OpenApp {

  def open(url: String) = {
    var os = System.getProperty("os.name").toLowerCase()
    val rt = Runtime.getRuntime()
    if (os.indexOf("win") >= 0) {
      rt.exec("rundll32 url.dll,FileProtocolHandler " + url)
    } else if (os.indexOf("mac") >= 0) {
      rt.exec("open " + url);
    } else if (os.indexOf("nix") >= 0 || os.indexOf("nux") >= 0) {

      rt.exec(Array(
        "sh", "-c",
        Seq("epiphany", "firefox", "mozilla", "konqueror", "netscape", "opera", "links", "lynx").mkString(" " + url + " || ")
      ))
    }
  }

}
