#' Extracts results from run R scripts
#'
#' @param project_dir The project directory
#' @param parcels list of data parcels
repbox_project_extract_r_results = function(project_dir, parcels=list(), opts=rbr_opts()) {
  restore.point("repbox_project_extract_r_results")

  parcels = repdb_load_parcels(project_dir, "r_chunk",parcels)

  # General outcomes
  dir = file.path(project_dir, "repbox/r/chunks")
  files = list.files(dir, glob2rx("out_*.Rds"),full.names = TRUE)

  out_df = lapply(files, readRDS) %>% bind_rows() %>% as_tibble() %>%
    repdb_null_to_empty("r_chunk_out")

  chunk_df = parcels$r_chunk$r_chunk %>%
    repdb_null_to_empty("r_chunk")

  out_df = left_join(out_df, select(chunk_df, script_num, chunkid), by=c("chunkid"))

  parcels$r_chunk_out = list(r_chunk_out=out_df)
  repdb_save_parcels(parcels["r_chunk_out"], file.path(project_dir, "repdb"))

  parcels = extractr_r_reg_results(project_dir, parcels)

  parcels
}

extractr_r_reg_results = function(project_dir, parcels=list()) {
  restore.point("extract_r_reg_results")

  # Extract regression information

  reg.dir = file.path(project_dir, "repbox/r/reg")
  files = list.files(reg.dir, glob2rx("call_*.Rds"),full.names = TRUE)
  if (length(files)==0) return(parcels)


  info = lapply(files, function(file) {
    res = readRDS(file)
    as_tibble(res$info_df)
  }) %>% bind_rows()

  files = list.files(reg.dir, glob2rx("glance_*.Rds"),full.names = TRUE)
  glance.li = lapply(files, readRDS)

  files = list.files(reg.dir, glob2rx("tidy_*.Rds"),full.names = TRUE)
  tidy.li = lapply(files, readRDS)

  info$reg.info = glance.li
  info$reg.tab = tidy.li

  # Save regression information in internal format
  saveRDS(info, file.path(project_dir, "repbox","r","reg_results.Rds"))

  # Transform into regDB specifications
  reg_df = info %>%
    rename(artid = project, runid=callid, cmd=fun, cmdline=call) %>%
    mutate(variant="rb", step = runid, lang="r", ncoef = sapply(reg.tab, NROW), nobs = sapply(reg.info, extract_glance_stat, var="nobs"), nobs_org = NA_integer_, timevar="", panelvar="", tdelta=NA_integer_, iv_code = 0, se_category=NA_character_, se_type = NA_character_, se_args = "", r2 = sapply(reg.info, extract_glance_stat, var="r.squared"))

  repdb_check_data(reg_df, "reg")

  regcoef = bind_rows_with_parent_fields(reg_df,"reg.tab",fields = c("artid","step","variant","cmd")) %>%
    rename(shown_term=term, coef = estimate, se = std.error, t = statistic, p = p.value,ci_low = conf.low, ci_up = conf.high) %>%
    mutate(label = "")

  regcoef$cterm=repboxReg::canonical.r.output.terms(regcoef$shown_term,vi=NULL, rcmd=regcoef$cmd, from.stata=FALSE)

  repdb_check_data(regcoef, "regcoef")

  parcels$r_reg = list(reg=reg_df, regcoef=regcoef)
  repdb_save_parcels(parcels["r_reg"], file.path(project_dir, "repdb"))
  parcels

}

extract_glance_stat = function(glance_df, var) {
  if (!var %in% names(glance_df)) return(NA)
  glance_df[[var]]

}
