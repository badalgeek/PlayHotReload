package io.badal.playCompiler

import java.io.File

/**
  * Created by badal on 11/25/15.
  */
class ServerSettings(val hostName: String,
                     val port: Integer,
                     val logDir: File,
                     val assetMap: Map[String, File])
