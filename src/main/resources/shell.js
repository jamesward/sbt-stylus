/* global process,require */


(function () {

    var args = process.argv,
        fs = require("fs"),
        basename = require('path').basename,
        resolve = require('path').resolve,
        join = require('path').join;

    // Import Stylus, expects it to be in the module path somewhere
    var stylus = require("stylus");

    function compileFile(file, dest, options) {

        // ensure file exists
        fs.lstat(file, function(err, stat) {
            if (err) throw err;
            // file
            if (stat.isFile()) {
                fs.readFile(file, 'utf8', function(err, str) {
                    if (err) throw err;
                    options.filename = file;
                    var engine = stylus(str, options);

                    if (options.includeCSS) style.set('include css', true);
                    if (options.resolveURL) style.set('resolve url', true);
                    
                    var engineWithPlugins = usePlugins(engine, options.plugins, resolve);
                    var engineWithPluginsAndImports = importFiles(engineWithPlugins, options._imports);

                    engineWithPluginsAndImports.render(function(err, css) {
                        if (err) throw err;
                    
                        var path = dest ? join(dest, basename(file, '.styl') + '.css') : file.replace(/\.styl$/i, '.css');
                        fs.writeFile(path, css, function(err){
                            if (err) throw err;
                        });
                    });
                });
                // directory
            } else if (stat.isDirectory()) {
                fs.readdir(file, function(err, files){
                    if (err) throw err;
                    files.filter(function(path){
                        return path.match(/\.styl$/);
                    }).map(function(path){
                        return join(file, path);
                    }).forEach(compileFile);
                });
            }
        });
    }


    function importFiles(style, imports) {
        imports.forEach(function(file) {
            style.import(file);
        });

        return style;
    }


    function usePlugins(style, plugins, resolve, urlFunction, resolveURL) {
        plugins.forEach(function(plugin) {
            var path = plugin.path;
            var options = plugin.options;
            var fn = require(/^\.+\//.test(path) ? resolve(path) : path);
            if ('function' != typeof fn) {
                throw new Error('plugin ' + path + ' does not export a function');
            }
            style.use(fn(options));
        });

        if (urlFunction) {
            style.define('url', stylus.url(urlFunction));
        } else if (resolveURL) {
            style.define('url', stylus.resolver());
        }

        return style;
    }
    
    
    if (typeof args[2] == 'undefined') throw new Error("input file(s) or directory required")

    var files = JSON.parse(args[2]);
    
    if (typeof args[3] == 'undefined') throw new Error("destination directory required")
    
    var dest = JSON.parse(args[3]);

    var options = {
        compress: false,
        firebug: false,
        linenos: false,
        includeCSS: false,
        resolveURL: false,
        paths: [],
        plugins: [],
        _imports: []
    }
    
    if (args[4]) {
        var parsedOptions = JSON.parse(args[4])
        for (c in parsedOptions) {
            options[c] = parsedOptions[c];
        }
    }
    
    // err.name = something
    // err.message = ./foo.styl:2
    
    var errors = [];
    
    files.forEach(function(file) {
        try {
            compileFile(file, dest, options);
        }
        catch (error) {
            console.log("asdf")
            errors.push(error);
        }
    });
    
    /*
    if (errors.length > 0) {
        errors.forEach(function(error) {
            console.error("name: " + error.name)
            console.error("message: " + error.message)
        })
    }
    */
    
}());




/*global process, require */

(function () {

    var args = process.argv,
        fs = require("fs"),
        path = require("path");

    // Import less, expects it to be in the module path somewhere
    var less = require("less");

    var mkdirp;

    var ensureDirectory = function (filepath) {
        var dir = path.dirname(filepath),
            cmd,
            existsSync = fs.existsSync || path.existsSync;
        if (!existsSync(dir)) {
            if (mkdirp === undefined) {
                try {
                    mkdirp = require('mkdirp');
                }
                catch (e) {
                    mkdirp = null;
                }
            }
            cmd = mkdirp && mkdirp.sync || fs.mkdirSync;
            cmd(dir);
        }
    };

    var jobs = JSON.parse(args[2]);
    var results = [];

    // Called when less has finished parsing a file
    var finishParsing = function (result) {
        results.push(result);
        if (jobs.length == results.length) {
            // If all files are passed, write the results to standard out
            console.log(JSON.stringify(results));
        }
    };

    // Called when an error is encountered
    var reportError = function (input, output, err) {
        finishParsing({status: "failure", input: input, output: output, compileErrors: [err]});
    };

    var doJob = function (options) {
        var input = options.input;
        if (options.verbose) {
            console.log("Compiling " + input);
        }

        var output = options.output;

        if (!options.sourceMapFileInline) {
            var writeSourceMap = function (output) {
                var filename = options.sourceMapFilename;
                ensureDirectory(filename);
                fs.writeFileSync(filename, output, 'utf8');
            };
        }

        // Most of this is adapted from the less bin/less script
        var parseLessFile = function (e, data) {

            if (e) {
                reportError(input, output, {message: "File not found"});
                return;
            }

            data = options.globalVariables + data + options.modifyVariables;

            options.paths = [path.dirname(input)].concat(options.paths);
            options.filename = input;

            var parser = new (less.Parser)(options);
            parser.parse(data, function (err, tree) {
                if (err) {
                    reportError(input, output, err);
                } else {
                    try {
                        var css = tree.toCSS({
                            silent: options.silent,
                            verbose: options.verbose,
                            ieCompat: options.ieCompat,
                            compress: options.compress,
                            cleancss: options.cleancss,
                            sourceMap: options.sourceMap,
                            sourceMapFilename: options.sourceMapFilename,
                            sourceMapURL: options.sourceMapURL,
                            sourceMapOutputFilename: options.sourceMapOutputFilename,
                            sourceMapBasepath: options.sourceMapBasepath,
                            sourceMapRootpath: options.sourceMapRootpath || "",
                            outputSourceFiles: options.outputSourceFiles,
                            writeSourceMap: writeSourceMap,
                            maxLineLen: options.maxLineLen,
                            strictMath: options.strictMath,
                            strictUnits: options.strictUnits
                        });
                        ensureDirectory(output);
                        fs.writeFile(output, css, 'utf8');
                        if (options.verbose) {
                            console.log("Wrote " + output);
                        }

                        var imports = [];
                        var files = parser.imports.files;
                        for (var file in files) {
                            if (files.hasOwnProperty(file)) {
                                imports.push(file);
                            }
                        }

                        finishParsing({status: "success", input: input, output: output, dependsOn: imports});
                    } catch (e) {
                        reportError(input, output, e);
                    }
                }
            });
        };

        fs.readFile(input, 'utf8', parseLessFile);

    };

    jobs.forEach(doJob);
}());
