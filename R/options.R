#' Creates options for repboxR
#'
#' @param ignore.sourced.files If TRUE, files that are sourced by other files will not be run.
#' @param just.files If not NULL, only files with names matching this pattern will be run. Used in function \code{repbox_project_run_r}.
#' @param verbose If TRUE, information about the progress of the analysis will be printed to the console.
#' @param use.log If TRUE, the output of the R scripts will be written to a log file.
#' @param use.spin If TRUE call knitr::spin to create HTML logs from R scripts.
#' @param extract.reg.info If TRUE extract information from regressions.
#' @param adapt.source.calls If TRUE, adapt calls to source to work nicely with repbox.
#' @param install.missing.libs If TRUE, install missing libraries.
#' @param r.bin The R binary command with path to use.
#' @param verbose.middleman If TRUE, print information inside middleman functions.
#' @param middleman.call.stack.skip How many calls to skip in the call stack when printing information inside middleman functions.
#' @param show.spin.log If TRUE shows output of spin calls. Mostly relevant when "source" call is replaced by a "spin" call.
#' @param recomplile.file.path.funs If TRUE always recompile the Rds file that specifies for which functions file paths shall be repaired using the source csv file. This argument will only be set TRUE during development.
repbox.r.opts = function(ignore.sourced.files=TRUE,just.files=NULL,verbose=TRUE, eval_mode = c("evaluate","source","spin")[1], use.log= (eval_mode=="source"), extract.reg.info=TRUE, adapt.source.calls=TRUE, install.missing.libs = FALSE, r.bin = "Rscript ", verbose.middleman = FALSE, middleman.call.stack.skip = 4, show.spin.log = TRUE, recompile.file.path.funs = TRUE, save_plot_rds=FALSE, invisible_max_char=500, visible_max_char=3000, ...) {
  opts = list(
    ignore.sourced.files=ignore.sourced.files,
    just.files = just.files,
    verbose=verbose,
    eval_mode = eval_mode,
    use.log = use.log,
    show.spin.log = show.spin.log,
    extract.reg.info = extract.reg.info,
    adapt.source.calls = adapt.source.calls,
    install.missing.libs = install.missing.libs,
    r.bin = r.bin,
    verbose.middleman = verbose.middleman,
    middleman.call.stack.skip = middleman.call.stack.skip,
    recompile.file.path.funs = recompile.file.path.funs,
    save_plot_rds = save_plot_rds,
    invisible_max_char=invisible_max_char,
    visible_max_char=visible_max_char,
    ...
  )
  opts
}

#' Retrieves options for repboxR
rbr.opts = function(name=NULL) {
  opts = getOption("repbox.r.options")
  if (is.null(name)) return(opts)
  opts[[name]]
}

set.rbr.opt = function(...) {
  args = list(...)
  opts = getOption("repbox.r.options")
  opts[names(args)] = args
  options(repbox.r.options=opts)
  invisible(opts)
}

repbox.env = function() {
  getOption("repbox.env")
}

repbox.rfile = function() {
  getOption("repbox.rfile")
}
repbox.output.counter = function() {
  getOption("repbox.output.counter")
}
repbox.project.dir = function() {
  getOption("repbox.project.dir")
}

repbox.sup.dir = function() {
  getOption("repbox.sup.dir")
}
