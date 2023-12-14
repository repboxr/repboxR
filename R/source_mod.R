# Functions that directly modify the R source files before running them
example = function() {
  project.dir = "~/repbox/projects2/testsupp"
  make.file.path.funs.rds("/home/rstudio/repbox/repboxR/inst/misc")
  r.file = "~/repbox/projects2/testsupp/org/code/test.R"
  modify.project.r.sources(project.dir)
}


get.file.path.funs.df = function(opts=rbs.opts()) {
  if (!opts$recompile.file.path.funs) {
    df = readRDS(system.file("misc/file_path_funs.Rds",package="repboxR"))
  } else {
    df = make.file.path.funs.rds(save=FALSE)
  }
  df
}

# Add information to file_path_funs.csv and store it as Rds file
make.file.path.funs.rds = function(save=TRUE) {
  out.dir = system.file("misc",package="repboxR")
  df = read.csv(system.file("misc/file_path_funs.csv",package="repboxR"))

  n = NROW(df)
  df$file.arg.pos = NA
  df$arg.def = vector("list", n)

  r = 1
  for (r in seq_len(n)) {
    pkg.fun.name = paste0(df$pkg[r],"::",df$fun[r])


    fun = try(eval(parse(text=pkg.fun.name)))
    if (is(fun, "try-error")) next
    df$arg.def[[r]] = names(as.list(args(fun)))
    df$file.arg.pos[[r]] = match(df$file.args[r],df$arg.def[[r]])
  }
  if (save) {
    rds.file = file.path(out.dir,"file_path_funs.Rds")
    saveRDS(df, rds.file)
  }
  invisible(df)
}


# si is the tibble returned from calling project.static.r.info
repbox_somo_modify_r_sources = function(project.dir,somo, opts) {
  restore.point("repbox_somo_modify_r_sources")
  sup.dir = normalizePath(file.path(project.dir, "mod"), mustWork = FALSE)

  call_pd = somo$call_pd
  called_funs = unique(call_pd$fun)

  if (opts$extract.reg.info) {
    reg.df = read.csv(system.file("misc/reg_funs.csv",package="repboxR"))
    rows = which(call_pd$fun %in% reg.df$fun)
    somo = somo_surround_calls(somo,
      pre="repbox.reg(",
      post=paste0(",",call_pd$id[rows],")"),
      call_pd_rows = rows)
  }

  path.df = get.file.path.funs.df(opts)
  path.df = path.df[path.df$fun %in% called_funs,]
  r = 2
  for (r in seq_len(NROW(path.df))) {
    fun = path.df$fun[r]
    rows = which(call_pd$fun == fun)

    somo = somo_surround_calls_arg(somo,
      call_pd_rows = rows,
      arg_name = path.df$file.args[r],
      arg_pos = path.df$file.arg.pos[r],
      arg_def = path.df$arg.def[[r]],
      pre = "repbox.path(",
      post=paste0(',"',path.df$mode[r],'",',call_pd$id[rows],")")
    )
  }

  if (opts$adapt.source.calls) {
    somo = somo_change_calls_fun_name(somo, "source","repbox.source", ignore_if_pkg = FALSE)
  }
  somo$code_df$mod_code = somo_make_code(somo)
  return(somo)


}


repbox_write_modified_r_sources = function(project.dir, somo) {
  restore.point("repbox_write_modified_r_sources")
  code_df = somo$code_df
  sup.dir = file.path(project.dir, "mod")
  files = file.path(sup.dir, code_df$file)
  for (i in seq_along(files)) {
    write_utf8(code_df$mod_code[[i]], files[[i]])
  }
}


# si is the tibble returned from calling project.static.r.info
modify.project.r.sources = function(project.dir, si, verbose=TRUE) {
  restore.point("modify.project.r.sources")
  org.dir = normalizePath(file.path(project.dir, "org"), mustWork = FALSE)
  r.files = si$org.rfile

  i = 1
  for (i in seq_along(r.files)) {
    r.file = r.files[i]
    if (verbose) {
      cat(paste0("\nAdapt code for ", basename(r.file)))
    }
    res = modify.repbox.r.source(r.file, somo=si$somo[[i]])
    new.code = res$new.code
    mod.file = file.path(project.dir, "mod",str.right.of(r.file, paste0(org.dir,"/")))
    write_utf8(new.code, mod.file)
  }
  return(invisible())
}

modify.repbox.r.source = function(r.file, somo=NULL, opts=rbr.opts()) {
  restore.point("modify.repbox.r.source")

  if (is.null(somo)) {
    code = readLines(r.file)
    somo = somo_init(code)
  } else {
    code = somo$code
  }
}
