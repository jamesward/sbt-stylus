package com.typesafe.stylus

import akka.actor.ActorRef
import akka.pattern.ask
import java.io.File
import spray.json._
import akka.util.Timeout
import scala.concurrent.{ExecutionContext, Future}
import com.typesafe.jse.Engine
import com.typesafe.jse.Engine.JsExecutionResult
import scala.util.control.NonFatal

/*
 * The Stylus options.
 *
 * @param inline Utilize image inlining via data URI support
 * @param compress Compress CSS output
 * @param firebug Emits debug infos in the generated CSS that can be used by the FireStylus Firebug plugin
 * @param lineNumbers Emits comments in the generated CSS indicating the corresponding Stylus line
 * @param includeCss Include regular CSS on @import
 * @param resolveUrl Resolve relative urls inside imports
 * @param use Utilize the Stylus plugin at <path>
 * @param include Add <path> to lookup paths
 * @param import Import stylus <file>
 */
case class StylusOptions()
/*
                          
                          inline: Boolean = false,
  compress: Boolean = false,
  firebug: Boolean = false,
  lineNumbers: Boolean = false,
  includeCss: Boolean = false,
  resolveUrl: Boolean = false,
  use: Option[String] = None,
  include: Option[String] = None,
  `import`: Option[File] = None)
  */

/** The result of compiling a Stylus file. */
sealed trait StylusResult {
  def input: File
  def output: File
}

/**
 * A successful Stylus compilation.
 */
case class StylusSuccess(input: File, output: File, dependsOn: Set[File]) extends StylusResult

/**
 * A compile error.
 */
case class StylusCompileError(filename: Option[File], line: Option[Int], column: Option[Int], message: String)

/**
 * An erroneous Stylus compilation.
 */
case class StylusError(input: File, output: File, compileErrors: Seq[StylusCompileError]) extends StylusResult

/**
 * The result of compiling many stylus files.
 */
case class StylusExecutionResult(results: Seq[StylusResult], stdout: String, stderr: String)

/**
 * JSON protocol for the Stylus result deserialisation.
 */
object StylusResultProtocol extends DefaultJsonProtocol {

  implicit object FileFormat extends RootJsonFormat[File] {
    def write(f: File) = JsString(f.getCanonicalPath)

    def read(value: JsValue) = value match {
      case s: JsString => new File(s.convertTo[String])
      case _ => deserializationError("String expected")
    }
  }

  implicit val StylusSuccess: RootJsonFormat[StylusSuccess] = jsonFormat3(StylusSuccess.apply)
  implicit val StylusCompileError = jsonFormat4(StylusCompileError.apply)
  implicit val StylusError = jsonFormat3(StylusError.apply)

  implicit val StylusResult = new JsonFormat[StylusResult] {
    def read(json: JsValue) = json.asJsObject.fields.get("status") match {
      case Some(JsString("success")) => json.convertTo[StylusSuccess]
      case Some(JsString("failure")) => json.convertTo[StylusError]
      case _ => throw new IllegalArgumentException("Unable to extract stylus result from JSON")
    }

    def writeStatus[C](status: String, c: C)(implicit format: JsonFormat[C]) = {
      JsObject(format.write(c).asJsObject.fields + ("status" -> JsString(status)))
    }

    def write(result: StylusResult) = result match {
      case success: StylusSuccess => writeStatus("success", success)
      case error: StylusError => writeStatus("failure", error)
    }
  }

}

/**
 * A stylus compiler
 *
 * @param engine The Javascript engine
 * @param shellSource The path of the Stylusc source file
 */
class StylusCompiler(engine: ActorRef, shellSource: File) {

  /**
   * Compile the given files using the given options.
   *
   * @param filesToCompile A tuple of input/output elements for files to compile
   * @param opts The stylus options
   * @param timeout The timeout
   * @return The result of the execution
   */
  def compile(filesToCompile: Seq[(File, File)], opts: StylusOptions)(implicit timeout: Timeout): Future[StylusExecutionResult] = {

    
    //'["./foo.styl"]' '{"paths": [], "plugins": [], "dest": "."}'
    
    val bool = JsBoolean.apply _

    val jsOptions: Map[String, JsValue] = Map(
      /*
      "paths" -> JsArray(opts.includePaths.map(p => JsString(p.getAbsolutePath)).toList),
    
      "paths" -> bool(opts.silent),
      "verbose" -> bool(opts.verbose),
      "ieCompat" -> bool(opts.ieCompat),
      "compress" -> bool(opts.compress),
      "cleancss" -> bool(opts.cleancss),
      "sourceMap" -> bool(opts.sourceMap),
      "sourceMapFileInline" -> bool(opts.sourceMapFileInline),
      "sourceMapStylusInline" -> bool(opts.sourceMapStylusInline),
      "max_line_len" -> JsNumber(opts.maxLineLen),
      "strictMath" -> bool(opts.strictMath),
      "strictUnits" -> bool(opts.strictUnits),
      "strictImports" -> bool(opts.strictImports),
      "optimization" -> JsNumber(opts.optimization),
      "color" -> bool(opts.color),
      "insecure" -> bool(opts.insecure),
      "relativeurls" -> bool(opts.relativeUrls),
      "globalVariables" -> JsString(""),
      "modifyVariables" -> JsString(""),
      
      "rootpath" -> JsString(opts.rootpath.getOrElse("")),
      "sourceMapRootpath" -> JsString(opts.sourceMapRootpath.getOrElse(""))
      */
    )

    import ExecutionContext.Implicits.global

    val options = JsArray()
    /*filesToCompile.toList.map {
      case (in, out) =>
        
        val inputOutputArgs = Map(
          "input" -> JsString(in.getAbsolutePath),
          "output" -> JsString(out.getAbsolutePath)
        )
        JsObject(jsOptions ++ inputOutputArgs)
    }).toString()
    */
    val args = List(options)
    (engine ? Engine.ExecuteJs(shellSource, args, timeout = timeout.duration)).map {
      case JsExecutionResult(exitValue, output, error) =>

        val stdout = new String(output.toArray, "UTF-8")
        val stderr = new String(error.toArray, "UTF-8")

        // Last line of the results should be the status
        val outputLines = stdout.split("\n")
        val status = outputLines.lastOption.getOrElse("{}")

        // Try and parse
        try {
          import StylusResultProtocol._
          val results = JsonParser(status).convertTo[Seq[StylusResult]]
          // Don't include the status line (ie the last line) in stdout, we exclude it by doing dropRight(1).
          def trimmed(out: Array[String]): String = if (out.isEmpty) "" else out.dropRight(1).mkString("\n")
          StylusExecutionResult(results, trimmed(outputLines), stderr)
        } catch {
          case NonFatal(e) =>
            val results = filesToCompile.map {
              case (in, out) => StylusError(
                in,
                out,
                Seq(StylusCompileError(None, None, None, s"Fatal error in stylus compiler: ${e.getMessage}"))
              )
            }
            StylusExecutionResult(results, new String(output.toArray), stderr)
        }
    }
  }
}
