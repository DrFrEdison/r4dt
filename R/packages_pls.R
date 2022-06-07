packages <- c("openxlsx"
                      , "data.table"
                      , "lubridate"
                      , "pls"
                      , "signal"
                      # , "mvdalab"
                      , "plyr"
                      , "R.utils"
                      , "chron"
                      , "prospectr"
                      , "plotly"
                      , "forecast"
                      , "readODS"
                      , "mdatools"
                      , "dplyr"
                      , "pbapply")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  invisible(lapply(packages, function(x) library(x, quietly = T, character.only = TRUE)))
)))
rm(packages, installed_packages)