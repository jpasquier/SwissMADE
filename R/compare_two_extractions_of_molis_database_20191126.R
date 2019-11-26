# Libraries
library(data.table)

# DT threads
setDTthreads(0)

# Working directory
setwd("~/Projects/SwissMADE")

# Load molis databases
load("data/PNR74_extractions_donnees_structurees_20190910/molis.rda")
molis_old <- molis
load("data/PNR74_extractions_donnees_structurees_20191112/molis.rda")

# Comparison 
sink("results/compare_two_extractions_of_molis_database_20191126.txt")
cat("============================================================\n")
cat(paste("Comparison of extractions of September 10, 2019 and November\n",
          "12, 2019 for the molis database\n"))
cat("============================================================\n\n")
cat(paste("Number of rows in the first database:", nrow(molis_old), "\n"))
cat(paste("Number of rows in the second database:", nrow(molis), "\n"))
cat(paste("Number of additional rows:", nrow(molis) - nrow(molis_old), "\n\n"))
v0 <- names(molis_old)[!(names(molis_old) %in% names(molis))]
if (length(v0) > 0) {
  v0 <- paste(v0, collapse = ", ")
} else {
  v0 <- "None"
}
v1 <- names(molis)[!(names(molis) %in% names(molis_old))]
if (length(v1) > 0) {
  v1 <- paste(v1, collapse = ", ")
} else {
  v1 <- "None"
}
cat(paste("Variables in the first database but not in the second:", v0, "\n"))
cat(paste("Variables in the second database but not in the first:", v1, "\n"))
sink()


