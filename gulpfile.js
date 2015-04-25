"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var jsvalidate = require("gulp-jsvalidate");

var paths = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

gulp.task("make", function() {
  return gulp.src(paths)
    .pipe(plumber())
    .pipe(purescript.pscMake());
});

gulp.task("jsvalidate", ["make"], function () {
  return gulp.src("output/**/*.js")
    .pipe(plumber())
    .pipe(jsvalidate());
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

["Prelude", "Prelude.Unsafe"].forEach(docTask);

gulp.task("docs", docTasks);

gulp.task("dotpsci", function () {
  return gulp.src(paths)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("default", ["jsvalidate", "docs", "dotpsci"]);
