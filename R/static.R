# The functions below use static code analysis to extract information
#
# Currently we want to know if other files are sourced
# and which packages are installed

repbox_r_static_make_parcels = function(project.dir, somo, script_nums, parcels=list()) {
  restore.point("repbox_r_static_make_parcels")

  artid = basename(project.dir)

  pd = somo$pd
  call_pd = somo$call_pd
  if (NROW(call_pd)==0) {
    call_pd = regdb_null_to_empty(call_pd, "r_call") %>%
      mutate(code_ind = integer(0), fun = character(0), id = integer(0), pkg = character(0))
  } else {
    call_pd = call_pd %>%
      mutate(script_num = script_nums[code_ind], artid=artid, code=text)
  }
  fun_df = call_pd %>%
    filter(fun=="function") %>%
    mutate(funid=id) %>%
    select(-fun)
  if (NROW(fun_df)>0) {
    fun_names_df = somo$as_df %>%
      filter(is_fun) %>%
      select(parent = id, fun=var)

    fun_df = left_join(fun_df, fun_names_df, by="parent")
  } else {
    fun_df = NULL
  }

  call_df = call_pd %>%
    filter(fun!="function") %>%
    mutate(callid = id)

  # Comments
  co_df = pd %>%
    filter(token == "COMMENT")


  if (NROW(co_df)>0) {


    # Determine whether we have a full line comment without
    # any other command
    no_co_df = pd %>%
      filter(token != "COMMENT") %>%
      select(code_ind, line1) %>%
      unique() %>%
      semi_join(co_df, by=c("code_ind","line1"))

    if (NROW(no_co_df)==0) {
      co_df$is_line_comment = rep(TRUE,NROW(co_df))
    } else {
      no_co_df$is_line_comment = FALSE
      co_df = left_join(co_df, no_co_df, by=c("code_ind","line1")) %>%
        mutate(is_line_comment = na.val(is_line_comment,TRUE))
    }

    # Determine which comment lines shall be merged
    co_df$merge_group = seq_rows(co_df)
    merge_with_prev =
      (lag(co_df$id)==co_df$id-1) &
      (lag(co_df$code_ind)==co_df$code_ind) &
      lag(co_df$is_line_comment) & co_df$is_line_comment
    for (i in which(is.true(merge_with_prev))) {
      co_df$merge_group[i] = co_df$merge_group[i-1]
    }
    com_df = co_df %>%
      group_by(code_ind, merge_group, funid) %>%
      summarize(
        line1 = min(line1),
        line2 = max(line2),
        text = paste0(text, collapse="\n"),
        funid = first(funid),
        id1 = first(id),
        id2 = last(id)
      )

    # Check if a comment ends before a function definition
    com_df$parent = com_df$id2 + 1

    if (NROW(fun_df)>0) {
      com_df = left_join(com_df, select(fun_df, parent, above_funid=funid), by="parent")
    } else {
      com_df$above_funid = NA_integer_
    }


    com_df$artid = rep(artid, NROW(com_df))
    com_df$script_num = script_nums[com_df$code_ind]
  } else {
    com_df = NULL
  }

  lib_calls = somo$lib.calls
  if (NROW(lib_calls)>0) {
    lib_calls = lib_calls %>%
      mutate(code_ind = somo$call_pd$code_ind[call_pd_row]) %>%
      select(code_ind, pkg=package)
  }

  pkg_df = bind_rows(
    select(call_df, code_ind,pkg),
    lib_calls
  ) %>%
    filter(!is.na(pkg)) %>%
    unique() %>%
    mutate(artid=artid, script_num = script_nums[code_ind])

  # Save source calls
  parcels = regdb_load_parcels(project.dir, "r_source",parcels)
  script_df = parcels$r_source$script_source %>%
    mutate(sourced_base = basename(file_path)) %>%
    select(sourced_script_num = script_num, sourced_base)

  # To do possibly deal with duplicate base file names
  if (NROW(somo$source_calls)>0) {
    source_calls = somo$source_calls %>%
      left_join(script_df, by="sourced_base") %>%
      mutate(caller_script_num = script_nums[caller_code_ind], artid=artid)
  } else {
    source_calls = NULL
  }


  regdb_check_data(fun_df, "r_fun")
  if (NROW(call_df)==0) call_df = NULL
  regdb_check_data(call_df, "r_call")
  regdb_check_data(com_df, "r_comment")
  regdb_check_data(pkg_df, "r_load_pkg")
  regdb_check_data(source_calls, "r_source_call")



  parcels$r_static = list(r_fun=fun_df, r_call=call_df, r_comment = com_df, r_load_pkg = pkg_df, r_source_call=source_calls)
  regdb_save_parcels(parcels[c("r_static")],dir = file.path(project.dir, "repbox","regdb"))

  parcels

}


repbox_r_somo_info = function(somo) {
  restore.point("repbox_r_somo_info")

  # Info about install packages commands
  # We don't extract all packages but only
  # the lines where packages are installed
  somo = static.info.install.pkgs(somo)

  # Store in somo$used.pkgs the package directly loaded
  # via a require or library command or via pkg::fun or pkg:::fun
  somo = static.info.used.pkgs(somo)

  # Find source info
  somo = somo_calls_info(somo,"source" ,"file",1)
  source.calls = somo$found
  if (NROW(source.calls)==0) {
    somo$source_calls = NULL
    return(somo)
  }


  source.calls = source.calls %>%
    mutate(
      caller_code_ind = somo$call_pd$code_ind[call_pd_row],
      file.is.const = str.is.quoted(file),
    )
  somo$has.variable.source = any(!source.calls$file.is.const)

  source.calls = source.calls %>%
    filter(file.is.const) %>%
    transmute(
      caller_code_ind = caller_code_ind,
      sourced_file = remove.quotes(file),
      sourced_base = basename(sourced_file),
      file_is_const = file.is.const
    )
  somo$source_calls = source.calls
  somo
}

static.info.used.pkgs = function(somo) {
  restore.point("static.info.used.pkgs")
  somo = static.info.library.require(somo)
  somo$used.pkgs = setdiff(unique(c(somo$call_pd$pkg, somo$lib.calls$package)),c(NA,""))
  somo
}

static.info.library.require = function(somo, fun = c("library", "require"), reset.libs=TRUE) {
  restore.point("static.info.library.require")

  if (reset.libs) {
    somo$lib.calls = NULL
  }

  if (length(fun)>1) {
    funs = fun
    for (fun in funs) {
      somo = static.info.library.require(somo, fun, reset.libs = FALSE)
    }
  }
  restore.point("static.info.library.require")

  somo = somo_calls_info(somo, fun, c("package","character.only"))

  lib.calls = somo$found

  if (NROW(lib.calls)==0) return(somo)

  lib.calls$quoted = str.is.quoted(lib.calls$package)

  lib.calls$character.only = as.logical(lib.calls$character.only)

  lib.calls = lib.calls %>%
    mutate(pkg = case_when(
      !is.true(character.only) ~ remove.quotes(package),
      quoted ~ remove.quotes(package),
      TRUE ~ NA_character_
    ))

  somo$lib.calls = bind_rows(somo[["lib.calls"]],lib.calls)
  somo

}

static.info.install.pkgs = function(somo) {
  restore.point("static.info.install.pkgs")
  # Find package installation commands
  call_pd = somo$call_pd
  if (NROW(call_pd)==0) {
    somo$does.install = FALSE
    return(somo)
  }



  remotes.funs =  c("install_bioc","install_bitbucket","install_cran","install_deps","install_dev","install_git","install_github","install_gitlab","install_local","install_svn","install_url","install_version")

  install.funs = c("install.packages",remotes.funs)

  ifuns.pd = filter(call_pd, fun %in% install.funs)

  ifuns.pd$pkg = ifelse(ifuns.pd == "install.packages","utils","remotes")

  somo$install.funs.pd = ifuns.pd

  if (NROW(ifuns.pd)==0) {
    somo$does.install = FALSE
    return(somo)
  }

  somo$does.install = TRUE
  return(somo)


}
