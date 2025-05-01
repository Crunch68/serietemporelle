
if(!("renv" %in% installed.packages())){
  install.packages("renv")
}

library(renv)



packages_to_install = c("rmarkdown", "knitr", "tidyverse", "readxl", "magrittr", 'reshape2',
                        "here", "openxlsx", "urca", "lmtest", "trend", "TSA", 'RColorBrewer', 
                        "CADFtest", "tsoutliers", "forecast", "lmtest", "Metrics", "TSstudio",
                        "FinTS")


install_and_load <- function(packages) {
  for (pkg in packages) { # Pour chaque package de la liste
    if (!requireNamespace(pkg, quietly = TRUE)) { # Vérifie si le package est installé ou non et 
      install.packages(pkg) # installe le cas échéant
    }
    library(pkg, character.only = TRUE) # Puis le charge
  }
}

install_and_load(packages_to_install)

renv::init(here())

