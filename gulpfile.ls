require! <[
  run-sequence 
  gulp 
  gulp-if 
  gulp-karma
  gulp-concat 
  gulp-purescript 
  gulp-filter
]>

paths =
  test:
    src: <[
      bower_components/chai/chai.js
      bower_components/
      bower_components/purescript-*/src/**/*.purs
      bower_components/purescript-*/src/**/*.purs.hs
      src/**/*.purs
    ]>
    dest: "tmp"

options =
  test:
    output: "Test.js"
    main: false
    externs: "extern.purs"

fil = gulp-filter (file) -> not /Test/ig.test file.path 

build = (k) -> ->
  x   = paths[k]
  o   = options[k]
  psc = gulp-purescript.psc o

  psc.on "error" ({message}) ->
    console.error message
    psc.end()

  gulp.src x.src 
    .pipe gulp-if /.purs/, psc
    .pipe gulp-concat o.output
    .pipe gulp.dest x.dest

gulp.task "test", build "test"

gulp.task "doc" ->
  gulp.src "src/**/*.purs"
    .pipe fil 
    .pipe gulp-purescript.docgen()
    .pipe gulp.dest "README.md"

gulp.task "travis"  <[test]>
