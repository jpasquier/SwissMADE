#!/usr/bin/env Rscript

db <- commandArgs(trailingOnly=TRUE)

library(data.table)
library(parallel)

setDTthreads(0)

setwd("~/Projects/SwissMADE/data-raw")

Dir <- "PNR74_extractions_donnees_structurees_20190910"
File <- paste0("pnr74_", db, "_result_filtered.csv")
f <- file.path(Dir, File)
assign(db, fread(f, sep = "|", encoding = "Latin-1"))

DB_prefix <- c(
  chop           = "chop",
  cim10          = "cim10",
  molis          = "molis",
  molis_smoker   = "molis_smoker",
  mouvement      = "mvt",
  ofs            = "ofs",
  predimed       = "medication",
  soarian_frcv   = "soarian_frcv",
  soarian_med    = "medication",
  soarian        = "soarian",
  soarian_sg_occ = "soa_sg_occ"
)

if (!(db %in% names(DB_prefix))) stop("prefix not found")
prefix <- DB_prefix[db]
X <- grep(paste0("^", prefix, "_"), names(get(db)), value = T)
k <- 0
continue <- TRUE
while (continue & k <= length(X)) {
  G <- do.call(rbind, lapply(combn(X, k, simplify = F), function(U) {
    U <- c("p_IPP", "sej_NUMERO_SEJOUR", U)
    data.table(variable = paste(U, collapse = ";"),
               n_dup = sum(duplicated(unique(get(db))[ , ..U])))
  }))
  ID <- G[n_dup == 0]
  if (any(G$n_dup == 0)) {
    continue <- F
  }
  k <- k + 1
}

saveRDS(ID, paste0(db, "_idvar.rds"))


