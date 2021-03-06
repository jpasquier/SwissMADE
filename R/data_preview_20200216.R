library(data.table)
library(magrittr)
library(writexl)

setwd("~/Projects/SwissMADE")

DB <- c("chop", "cim10", "molis_smoker", "molis", "mouvement",
        "ofs", "predimed", "soarian_frcv", "soarian_med",
        "soarian_sg_occ", "soarian")

tbls <- lapply(setNames(DB, DB), function(db) {
  load(paste0("data/", db, ".rda"))
  r <- data.frame(variable = names(get(db)),
                  type = sapply(get(db), function(z) class(z)[1]))
  n <- 30
  v <- t(sapply(get(db), function(z) {
    if (class(z)[1] %in% c("Date", "POSIXct")) z <- as.character(z)
    unique(na.omit(z))[1:n]
  }))
  colnames(v) <- paste0("value", 1:n)
  r <- cbind(r, v)
  rm(list = db)
  return(r)
})

write_xlsx(tbls, "results/data_preview_20200216.xlsx")
