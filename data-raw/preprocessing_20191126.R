# Libraries
library(data.table)

# DT threads
setDTthreads(0)

# Working directory
setwd("~/Projects/SwissMADE")

# Raw data directory
Dir0 <- "data-raw/PNR74_extractions_donnees_structurees_20190910"

# Data directory
Dir1 <- sub("data-raw", "data", Dir0)

# Load databases and store them as RData files
Files <- list.files(Dir0)
DB <- sub("_result_filtered.csv.xz$", "", sub("^pnr74_", "", Files))
for (db in DB) {
  f0 <- file.path(Dir0, paste0("pnr74_", db, "_result_filtered.csv.xz"))
  assign(db, fread(cmd = paste("unxz -cq", f0),
                   sep = "|", encoding = "Latin-1"))
  f1 <- file.path(Dir1, paste0(db, ".rda"))
  save(list = db, file = f1, version = 2, compress = "xz")
  rm(list = db)
}
rm(Dir0, Dir1, Files, DB, db, f0, f1)


