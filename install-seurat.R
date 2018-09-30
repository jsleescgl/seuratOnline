#!/usr/bin/env Rscript

install.packages("shinydashboard")
install.packages("shinyjs")
install.packages("shinyBS")
install.packages("shinycssloaders")
install.packages('DT')

#In (function (dep_name, dep_ver = NA, dep_compare = NA)  :
#  Need Seurat == 2.3.0 but loaded version is 2.3.4

library(devtools);
devtools::load_all()
# devtools::install_github("aymanm/seuratOnline");