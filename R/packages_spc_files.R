# List of packages needed for LG3 and LG2 ####
packages <- c("hyperSpec","data.table","plotly","png","rsvg","htmlwidgets","plyr","tools", "prospectr", "chron","pspline","anytime","Rcpp")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  invisible(lapply(packages, function(x) library(x, quietly = T, character.only = TRUE)))
)))
rm(packages, installed_packages)