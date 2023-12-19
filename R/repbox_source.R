# Experimental code:
#
# Goal: A source replacement that can generate knitr output put
# at the same time keeps source references.

example = function() {
  rfile = "~/repbox/projects2/testsupp/mod/code/test2.R"
}

mysource = function() {
  library(evaluate)
  con = file(rfile, open = "r")
  pdf = parse_all(con)
  close(con)
  pdf$src
  attributes(pdf$expr[[1]])
  evaluate:::default_output_handler

  attributes(calls)
  attributes(calls[[1]])
  info = getParseData(calls)
  getParseText(calls)
}


repbox.source.r = function(rfile,project_dir, env=repbox.env(), opts=rbr_opts()) {
  restore.point("repbox.source.r")
  options(repbox.rfile = rfile)
  id = tools::file_path_sans_ext(basename(rfile))
  calls = parse(file = rfile, keep.source=TRUE)

  #repbox.source(rfile)
  if (!opts$use.spin) {
    if (opts$use.log) {
      log.file = file.path(project_dir, paste0("repbox/r/log/",id,".log"))
      if (file.exists(log.file)) file.remove(log.file)
      con <- file(log.file)
      sink(con, append=TRUE)
      sink(con, append=TRUE, type="message")
    }

    try(source(rfile,env, echo=TRUE, keep.source=TRUE))
    if (opts$use.log) {
      try(sink(), silent=TRUE)
      try(sink(type="message"), silent = TRUE)
      try(close(con))
    }
  } else if (opts$use.spin) {
    log.file = file.path(project_dir, paste0("repbox/r/log/",id,".html"))
    knitr::stitch_rhtml(script=rfile,output=log.file, envir = env)
  }
}
