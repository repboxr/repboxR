#' A chunk shall be a top level expression in an R script that would be
#' evaluated as a single expression in the R console
#' E.g. a single line or expressions in {} including loops or if statements
#' We also perform all code replacements and store them in chunk_df$mod_code

repbox_somo_to_chunk_df = function(somo, script_nums) {
  restore.point("repbox_somo_to_chunk_df")
  pd = somo$pd
  rows = which(pd$parent<0)
  chunk_df = pd[rows,]
  chunk_df$mod_code = somo_make_pd_code(somo, rows)
  chunk_df$chunkid = chunk_df$id
  chunk_df$script_num = script_nums[chunk_df$code_ind]
  chunk_df
}

repbox_save_slim_chunk_dfs = function(project.dir, chunk_df) {
  restore.point("repbox_save_slim_chunk_dfs")
  out.dir = file.path(project.dir, "repbox","r","chunks")
  if (!dir.exists(out.dir)) dir.create(out.dir)
  script_nums = unique(chunk_df$script_num)
  chunk_df = chunk_df[,c("script_num","chunkid","mod_code")]
  for (script_num in script_nums) {
    cdf = chunk_df[chunk_df$script_num==script_num,]
    file=paste0(out.dir,"/chunks_",script_num,".Rds")
    saveRDS(cdf, file)
  }
}
