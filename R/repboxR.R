example = function() {
  library(repboxRun)
  library(repboxR)

  parcels = list()
  project_dir = "C:/libraries/repbox/projects_reg/testr"
  project_dir = "C:/libraries/repbox/projects_dv/BPON3K"
  steps = repbox_run_steps_from(static_code=TRUE, art=FALSE)
  repbox_run_project(project_dir,lang=NULL, steps=steps)
  rstudioapi::filesPaneNavigate(project_dir)

  opts = repbox.r.opts(ignore.sourced.files = TRUE, use.log=FALSE, extract.reg.info = TRUE, #just.files="tab_1_summary_stats.R"
  )

  parcels = repbox_project_static_analyse_r(project_dir, opts=opts)
  parcels = repbox_project_run_r(project_dir, parcels=parcels, opts=opts)
  parcels = repbox_project_extract_r_results(project_dir, parcels, opts=opts)

  html = html_all_r(project_dir)
  html.dir = file.path(project_dir,"reports")
  repbox_save_html(html %>% repbox_add_html_header(), "r_code.html", html.dir)
  rstudioapi::filesPaneNavigate(html.dir)


  rstudioapi::filesPaneNavigate(project_dir)

  rstudioapi::filesPaneNavigate("~/repboxR/R")
  rstudioapi::filesPaneNavigate("C:/libraries/sourcemodify")
  rstudioapi::filesPaneNavigate("C:/libraries/repbox/repboxR/R")
  rstudioapi::filesPaneNavigate("C:/libraries/repbox/repboxDB/inst/regdb")
}

repbox_project_analyse_r = function(project_dir,parcels=list(), opts=repbox.r.opts()) {
  restore.point("repbox_project_analyse_r")
  parcels = repbox_project_static_analyse_r(project_dir,parcels=parcels, opts=opts)
  parcels = repbox_project_run_r(project_dir, parcels=parcels, opts=opts)
  parcels = repbox_project_extract_r_results(project_dir, parcels, opts=opts)
  parcels
}



#' Perform static analysis of R code files
repbox_project_static_analyse_r = function(project_dir,parcels=list(), opts=repbox.r.opts()) {
  restore.point("repbox_project_static_analyse_r")

  parcels = regdb_load_parcels(project_dir,"r_source",parcels)
  source_df = parcels$r_source$script_source
  script_nums = source_df$script_num[source_df$file_type=="r"]
  somo = somo_init(code=source_df$text, files=source_df$file_path)

  pd = somo$pd

  somo = repbox_r_somo_info(somo)

  parcels = repbox_r_static_make_parcels(project_dir, somo, script_nums, parcels)
  parcels$.somo = list(somo=somo)
  parcels
}



#' Run R code files
repbox_project_run_r = function(project_dir,parcels=list(), opts=repbox.r.opts()) {
  restore.point("repbox_project_run_r")
  verbose = opts$verbose

  repbox.dir = file.path(project_dir,"repbox/r")
  if (!dir.exists(repbox.dir)) dir.create(repbox.dir,recursive = TRUE)


  # Clear log, chunk, reg and figure directories
  del.dirs = paste0(project_dir, "/repbox/r/", c("log","chunks","figure","reg"))
  del.files = list.files(del.dirs,full.names = TRUE)
  file.remove(del.files)


  parcels = regdb_load_parcels(project_dir, c("r_static","r_source"))
  source_df = parcels$r_source$script_source

  # No R script found. Possibly static analysis did not run
  if (NROW(source_df)==0) {
    rfiles = list.files(file.path(project_dir, "org"),glob2rx("*.r"),full.names = TRUE,recursive = TRUE,ignore.case = TRUE)
    if (length(rfiles)==0) {
      if (opts$verbose)
        cat(paste0("\n  No R code files found in ", project_dir,"\n"))
      return(parcels)
    }


    # Run static code analysis
    parcels = repbox_project_static_analyse_r(project_dir,parcels, opts)
    source_df = parcels$r_source$script_source
  }

  if (!is.null(opts$just.files)) {
    source_df = source_df[basename(source_df$file_path) %in% basename(opts$just.files),]
  }
  script_nums = source_df$script_num[source_df$file_type=="r"]
  if (!is.null(parcels$.somo) & is.null(opts$just.files)) {
    somo = parcels$.somo$somo
  } else {
    somo = somo_init(code=source_df$text, files=source_df$file_path)
  }

  project = basename(project_dir)
  sup.dir = file.path(project_dir, "mod")

  opts$project_dir = project_dir
  opts$sup.dir = sup.dir

  rows = which(source_df$file_type == "r")
  opts$script_df = as.data.frame(source_df[rows,c("script_num","file_path")])

  saveRDS(opts, file.path(project_dir,"repbox/r/r_opts.Rds"))

  if (!dir.exists(sup.dir)) {
    org.dir = file.path(project_dir, "org")
    copy.dir(org.dir, sup.dir,copy.date=TRUE)
    unzip.zips(sup.dir)
  }

  if (opts$install.missing.libs) {
    pkg_df = parcels$r_static$r_load_pkg
    libs = unique_no_na(pkg_df$pkg)
    install.missing.r.libs(libs)
  }

  # Modify R source files to include
  # path correction and regression info storage
  somo = repbox_somo_modify_r_sources(project_dir,somo, opts)
  repbox_write_modified_r_sources(project_dir, somo)

  # Create chunk_df with modified source code
  chunk_df = repbox_somo_to_chunk_df(somo, script_nums)
  repbox_save_slim_chunk_dfs(project_dir, chunk_df)
  parcels$r_chunk = list(r_chunk=chunk_df)
  regdb_save_parcels(parcels[c("r_chunk")], file.path(project_dir,"repbox/regdb"))


  # Ignore files that are sourced in other files
  source_df$ignore = FALSE

  if (opts$ignore.sourced.files) {
    sc_df = parcels$r_static$r_source_call
    if (NROW(sc_df)>0) {
      ignore.rows = source_df$script_num %in% sc_df$sourced_script_num
      source_df$ignore[ignore.rows] = TRUE
    }
  }

  # Reset log of file.path() calls
  writeLines(
    "callid,mode,found_file,org_file,sup_dir,wdir,type",
    file.path(repbox.dir,"find_path_log.csv")
  )


  rfiles =  file.path(sup.dir, source_df$file_path[!source_df$ignore])
  if (NROW(rfiles)==0) {
    if (verbose) cat("\nNo R files to analyse")
    return(NULL)
  }
  script_nums = source_df$script_num[!source_df$ignore]

  if (verbose)
    cat("\nReplicate ", project, " with ", NROW(rfiles), " R files.\n")

  clear.and.create.dir(file.path(repbox.dir,"reg"))
  clear.and.create.dir(file.path(repbox.dir,"log"))

  for (i in seq_along(rfiles)) {
    rfile = rfiles[i]
    cat(paste0("\nRun ",i, " of ", NROW(rfiles),": ", rfile,"\n"))
    repbox.run.r.file(rfile,script_num = script_nums[i], project_dir, opts=opts)
  }
  return(parcels)
}

repbox.make.r.run.script = function(rfile, script_num, project_dir, opts=rbr.opts()) {
  restore.point("repbox.make.r.run.script")
  sup.dir = file.path(project_dir,"mod")
  opts.file = file.path(project_dir,"repbox/r/r_opts.Rds")
  glob_head_code = paste0('
suppressMessages(library(repboxRfun))
opts = readRDS("', opts.file, '")
setwd("',sup.dir,'")
options(.repbox.rfile = "',rfile,'")
options(.repbox.options = opts)
')

  head_code = tail_code = ""


  #id = tools::file_path_sans_ext(basename(rfile))
  id = script_num

  if (opts$eval_mode == "evaluate") {
    main_code = paste0('
env = new.env(parent=globalenv())
options(.repbox.env = env)
repbox_evaluate_script_chunks("',project_dir,'",', script_num, ', env)
')

  } else if (opts$eval_mode == "source") {
    log.file = file.path(project_dir, paste0("repbox/r/log/",id,".log"))
    if (file.exists(log.file)) file.remove(log.file)
head_code = paste0('
con <- file("',log.file,'")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")')

tail_code = paste0('
try(close(con), silent=TRUE)
')
    main_code = paste0('
source("',rfile,'", echo=TRUE, keep.source=TRUE)
')

  } else if (opts$eval_mode == "spin") {
    main_code = paste0('
env = new.env(parent=globalenv())
options(.repbox.env = env)
repbox.stitch.source("', rfile, '")
')
  } else {
    stop(paste0("Unknown evaluation mode ",opts$eval_mode))
  }

  code = paste0(glob_head_code, head_code, main_code, tail_code)
  temp.file = file.path(project_dir, "repbox/r/run_script.R")
  writeLines(code, temp.file)
  return(temp.file)
}

repbox.run.r.file = function(rfile, script_num, project_dir, opts=rbr.opts()) {
  restore.point("repbox.run.r.file")
  temp.file = repbox.make.r.run.script(rfile, script_num, project_dir, opts)
  cmd = paste0(opts$r.bin, '"',temp.file,'"')
  system(cmd,wait=TRUE)

}




get.project.org.rfiles = function(project_dir) {
  rfiles = list.files(file.path(project_dir,"org"), glob2rx("*.r"), ignore.case=TRUE, full.names=TRUE, recursive=TRUE)
  rfiles
}

