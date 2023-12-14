write_utf8 = function(x, file, bom=F) {
  con <- file(file, "wb")
  if(length(x)>1) x = paste0(x, collapse="\n")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}

unique_no_na = function(x) {
  setdiff(unique(x),NA)
}

str.is.quoted = function(str) {
  startsWith(str,"'") | startsWith(str,'"')
}

remove.quotes = function(str) {
  rows = which(is.true(startsWith(str,'"') | startsWith(str,"'")))
  if (length(rows)==0) return(str)

  str[rows] = substring(str[rows], 2, nchar(str[rows])-1)
  str
}

is.true = function(val) {
  val[is.na(val)] = FALSE
  return(as.logical(val))
}

add.col = function(df, col, val) {
  if (is.null(df)) return(NULL)
  if (NROW(df)==0) {
    val = character(0)
  }
  df[[col]] = val
  df
}

clear.and.create.dir = function(clear.dir,recursive=TRUE) {
  restore.point("clear.and.create.dir")
  if (dir.exists(clear.dir)) {
    unlink(clear.dir, recursive=TRUE)
  }
  if (!dir.exists(clear.dir))  dir.create(clear.dir, recursive = TRUE)
}

