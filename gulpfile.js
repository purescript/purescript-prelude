/* jshint node: true */
"use strict";

var gulp = require("gulp");
var jshint = require("gulp-jshint");
var jscs = require("gulp-jscs");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var run = require("gulp-run");

var paths = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

gulp.task("lint", function() {
  return gulp.src("src/**/*.js")
    .pipe(jshint())
    .pipe(jshint.reporter())
    .pipe(jscs());
});

gulp.task("make", ["lint"], function() {
  return gulp.src(paths)
    .pipe(plumber())
    .pipe(purescript.pscMake());
});

var docTasks = [];

var docTask = function(name) {
  var taskName = "docs-" + name.toLowerCase();
  gulp.task(taskName, function () {
    return gulp.src("src/" + name.replace(/\./g, "/") + ".purs")
      .pipe(plumber())
      .pipe(purescript.pscDocs())
      .pipe(gulp.dest("docs/" + name + ".md"));
  });
  docTasks.push(taskName);
};

["Prelude"].forEach(docTask);

gulp.task("docs", docTasks);

gulp.task("dotpsci", function () {
  return gulp.src(paths)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("test", function() {
  return gulp.src(paths.concat(["test/Main.purs"]))
    .pipe(plumber())
    .pipe(purescript.psc({ main: "Test.Main" }))
    .pipe(run("node"));
});

gulp.task("default", ["make", "docs", "dotpsci"]);
