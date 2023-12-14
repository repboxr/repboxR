# Functions to replicate / improve on the reproduction study Trisovic et al (2022) in
# Nature - Scientific Data who study 2000 R code supplements on Dataverse.
#
# https://www.nature.com/articles/s41597-022-01143-6
#
# With Github package here:
#
# https://github.com/atrisovic/dataverse-r-study
#
# They use a simpler static path correction scheme using regular
# expression matching and reduce the failure rate from 74% to 56%.
#
# It would be interesting how well our dynamic path correction
# performs in comparison.

# Part of these functions might be moved to repboxMain

example = function() {
  dv = read.csv("~/repbox/projects_dv/aggregate_results_env.csv")
  dv.fail = filter(dv, success==0)
  dois = dv.fail$doi %>% unique()
  doi = dois[1]
  make.dv.project(doi)
}

init.repbox.dv.project = function(doi, projects.dir = "~/repbox/projects_dv", downloads.dir = "~/repbox/dataverse") {
  restore.point("make.dv.project")
  project = str.right.of(doi,"10.7910/DVN/",not.found = NA)
  if (is.na(project)) {
    stop("Provided doi is no default DATAVERSE doi.")
  }
  zip.file = download.dataverse.zip(doi, downloads.dir, overwrite=FALSE)
  project.dir = file.path(projects.dir,project)
  #if (!dir.exists(project.dir)) dir.create(project.dir)
  #unzip(zip.file,exdir=file.path(project.dir,"org"))

  repboxMain::init.repbox.project(project.dir, sup.zip = zip.file)
}

download.dataverse.zip = function(doi, downloads.dir = "~/repbox/dataverse_zip", overwrite = FALSE) {
  project = str.right.of(doi,"10.7910/DVN/",not.found = NA)
  url = paste0("http://dataverse.harvard.edu/api/access/dataset/:persistentId/?persistentId=", doi)
  destfile = paste0(downloads.dir,"/", project,".zip")
  if (file.exists(destfile) & !overwrite)
    return(destfile)
  download.file(url, destfile=destfile)
}
