# Functions to find package dependencies in R files
# of a code supplement and to install them.

example = function() {
  project.dir = "~/repbox/projects_dv/BPON3K"
  libs = find.project.r.libs(project.dir)
  install.missing.r.libs(libs[2])
  install.missing.r.libs(libs)
}

install.missing.r.libs = function(libs, verbose=FALSE) {
  for (lib in libs) {
    if (!require(lib,character.only = TRUE)) {
      install.packages(lib)
    } else {
      if (verbose)
        cat("\nPackage", lib,"is already installed.\n")
    }
  }
}



# Find all R package dependencies of a code supplement
# Not used anymore. Replaced by analysis in  static.R

find.project.r.libs = function(project.dir, sup.dir = file.path(project.dir,"org")) {
  rfiles = list.files(sup.dir, glob2rx("*.r"), ignore.case=TRUE, full.names=TRUE, recursive=TRUE)
  libs = lapply(rfiles, find.r.libs) %>%
    unlist() %>%
    unique()
  libs
}

# Find package dependencies in an R file using automagic
# Not used anymore. Replaced by analysis in  static.R

find.r.libs = function(rfile) {
  automagic::parse_packages(rfile)
}

