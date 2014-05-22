package com.typesafe.stylus.sbt

import sbt.Keys._
import sbt._
import sbt.KeyRanks._
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import com.typesafe.jse.{Rhino, PhantomJs, Node, CommonNode, Trireme}
import com.typesafe.jse.sbt.JsEnginePlugin.JsEngineKeys
import com.typesafe.web.sbt.WebPlugin.WebKeys
import xsbti.{CompileFailed, Severity, Problem}
import com.typesafe.web.sbt.{WebPlugin, GeneralProblem, LineBasedProblem}
import akka.util.Timeout
import scala.collection.immutable
import com.typesafe.web.sbt.CompileProblems
import com.typesafe.web.sbt.incremental
import com.typesafe.web.sbt.incremental._
import com.typesafe.stylus.{StylusError, StylusOptions, StylusSuccess, StylusCompileError}

object StylusPlugin extends sbt.Plugin {

  object StylusKeys {
    // Stylus command
    val stylus = TaskKey[Seq[File]]("stylus", "Invoke the Stylus compiler.")

    val stylusFilter = SettingKey[FileFilter]("stylus-filter", "The filter for Stylus files.")
    val stylusSources = TaskKey[PathFinder]("stylus-sources", "The list of Stylus sources.", ASetting)

    // Internal
    val stylusSource = TaskKey[File]("stylus-source", "The extracted Stylus source file.", CSetting)
    val stylusOptions = TaskKey[StylusOptions]("stylus-options", "The Stylus options", CSetting)
    //val partialstylusOptions = TaskKey[(Int, Boolean, Boolean, Boolean, Int, Boolean, Boolean, Boolean) => stylusOptions]("stylus-options-partial")
    
    val inline = SettingKey[Boolean]("stylus-inline", "Utilize image inlining via data URI support", ASetting)
    val compress = SettingKey[Boolean]("stylus-compress", "Compress CSS output", ASetting
    val firebug = SettingKey[Boolean]("stylus-firebug", "Emits debug infos in the generated CSS that can be used by the FireStylus Firebug plugin", ASetting)
    val lineNumbers = SettingKey[Boolean]("stylus-line-numbers", "Emits comments in the generated CSS indicating the corresponding Stylus line", ASetting)
    val includeCss = SettingKey[Boolean]("stylus-include-css", "Include regular CSS on @import", ASetting)
    val resolveUrl = SettingKey[Boolean]("stylus-resolve-url", "Resolve relative urls inside imports", ASetting)
    val use = SettingKey[Option[String]]("stylus-use", "Utilize the Stylus plugin at <path>", ASetting)
    val include = SettingKey[Option[String]]("stylus-include", "Add <path> to lookup paths", ASetting)
    val `import` = SettingKey[Option[File]]("stylus-import", "Import stylus <file>", ASetting)
  }

  import StylusPlugin._
  import JsEngineKeys._
  import WebKeys._

  private val defaults = StylusOptions()

  private val unscopedSettings = Seq(
    inline := defaults.inline,
    compress := defaults.compress,
    firebug := defaults.firebug,
    lineNumbers := defaults.lineNumbers,
    includeCss := defaults.includeCss,
    resolveUrl := defaults.resolveUrl,
    use := defaults.use,
    include := defaults.include,
    `import` := defaults.`import`,
  
  

    includePathGenerators := Nil,
    includePathGenerators <+= includePaths.map(identity),
    includePathGenerators <+= webJars.map(Seq(_)),
    allIncludePaths <<= Defaults.generate(includePathGenerators),

    // Using a partial here given that we need to work around param # limitations.
    partialstylusOptions <<= (silent, verbose, ieCompat, compress, cleancss, allIncludePaths, sourceMap, sourceMapstylusInline,
      sourceMapFileInline, sourceMapRootpath, rootpath).map {
      (s, v, ie, co, cl, ip, sm, sl, sf, sr, r) =>
        stylusOptions(s, v, ie, co, cl, ip, sm, sl, sf, sr, r, _, _, _, _, _, _, _, _)
    },

    stylusOptions <<= (partialstylusOptions, maxLineLen, strictMath, strictUnits, strictImports, optimization, color,
      insecure, relativeUrls).map {
      (partialstylusOptions, mll, sm, su, si, o, c, i, rl) =>
        partialstylusOptions(mll, sm, su, si, o, c, i, rl)
    },

    stylusSources := (unmanagedSources.value ** stylusFilter.value).get
  )

  def stylusSettings = Seq(
    styluscSource in LocalRootProject <<= (target in LocalRootProject).map {
      target =>
        WebPlugin.copyResourceTo(
          target / "stylus-plugin",
          "stylusc.js",
          stylusPlugin.getClass.getClassLoader
        )
    },

    stylusFilter in Assets := GlobFilter("*.stylus"),
    stylusFilter in TestAssets := (stylusFilter in Assets).value,

    stylus in Assets <<= stylusTask(Assets),
    stylus in TestAssets <<= stylusTask(TestAssets),
    stylus <<= stylus in Assets,

    compile in Compile <<= (compile in Compile).dependsOn(stylus in Assets),
    test in Test <<= (test in Test).dependsOn(stylus in Assets, stylus in TestAssets)

  ) ++ inConfig(Assets)(unscopedSettings) ++ inConfig(TestAssets)(unscopedSettings)

  private def stylusTask(scope: Configuration) = (state, styluscSource in LocalRootProject, nodeModules in Plugin,
    unmanagedSourceDirectories in scope, stylusSources in scope, resourceManaged in scope,
    stylusOptions in scope, engineType, streams, reporter, parallelism).map(stylusCompiler)

  /**
   * The stylus compiler task
   */
  def stylusCompiler(state: State,
                   stylusc: File,
                   nodeModules: File,
                   sourceFolders: Seq[File],
                   stylusSources: PathFinder,
                   outputDir: File,
                   stylusOptions: StylusOptions,
                   engineType: EngineType.Value,
                   s: TaskStreams,
                   reporter: LoggerReporter,
                   parallelism: Int
                    ): Seq[File] = {

    import com.typesafe.web.sbt.WebPlugin._

    val timeoutPerSource = 10.seconds

    val engineProps = engineType match {
      case EngineType.CommonNode => CommonNode.props()
      case EngineType.Node => Node.props(stdModulePaths = immutable.Seq(nodeModules.getCanonicalPath))
      case EngineType.PhantomJs => PhantomJs.props()
      case EngineType.Rhino => Rhino.props()
      case EngineType.Trireme => Trireme.props(stdModulePaths = immutable.Seq(nodeModules.getCanonicalPath))

    }

    outputDir.mkdirs()

    val files = (stylusSources.get x relativeTo(sourceFolders)).map {
      case (file, relative) =>
        // Drop the .stylus, and add either .css or .min.css
        val extension = if (stylusOptions.compress) ".min.css" else ".css"
        val outName = relative.replaceAll("\\.stylus$", "") + extension

        val out = outputDir / outName
        file -> out
    }

    implicit val opInputHasher =
      OpInputHasher[(File, File)](sourceMapping => OpInputHash.hashString(sourceMapping + "|" + stylusOptions))

    val problems: Seq[Problem] = incremental.runIncremental(s.cacheDirectory, files) {
      modifiedFiles: Seq[(File, File)] =>

        if (modifiedFiles.size > 0) {
          s.log.info(s"Compiling ${modifiedFiles.size} stylus files")

          val fileBatches = (modifiedFiles grouped Math.max(modifiedFiles.size / parallelism, 1)).toSeq
          val pendingResultBatches: Seq[Future[Seq[StylusResult]]] = fileBatches.map {
            sourceBatch =>
              implicit val timeout = Timeout(timeoutPerSource * sourceBatch.size)
              withActorRefFactory(state, this.getClass.getName) {
                arf =>
                  val engine = arf.actorOf(engineProps)
                  val compiler = new StylusCompiler(engine, stylusc)
                  compiler.compile(sourceBatch, stylusOptions).map {
                    result =>
                      if (!result.stdout.isEmpty) s.log.info(result.stdout)
                      if (!result.stderr.isEmpty) s.log.error(result.stderr)
                      result.results
                  }
              }
          }

          val allstylusResults = Await.result(Future.sequence(pendingResultBatches), timeoutPerSource * modifiedFiles.size).flatten

          val results: Map[(File, File), OpResult] = allstylusResults.map {
            entry =>
              val result = entry match {
                case StylusSuccess(input, output, dependsOn) =>
                  OpSuccess(dependsOn + input, Set(output))
                case _ => OpFailure
              }
              (entry.input -> entry.output) -> result
          }.toMap

          def lineContent(file: File, line: Int) = IO.readLines(file).drop(line - 2).headOption

          val problems = allstylusResults.map {
            case e: StylusError =>
              e.compileErrors.map {
                case StylusCompileError(Some(input), Some(line), Some(column), message) =>
                  new LineBasedProblem(message, Severity.Error, line, column - 1, lineContent(input, line).getOrElse(""), input)

                case StylusCompileError(Some(input), _, _, message) =>
                  new GeneralProblem(message, input)
              }
            case _ => Nil
          }.flatten.distinct

          (results, problems)

        } else {
          (Map.empty, Seq.empty)
        }
    }

    CompileProblems.report(reporter, problems)

    files.map(_._2)
  }

}