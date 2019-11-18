# Librairies
library(data.table)
library(parallel)
library(magrittr)
library(writexl)

# DT threads
setDTthreads(0)

# Working directory
setwd("~/Projects/SwissMADE")

# Load databases
txz_file <- "PNR74_extractions_donnees_structurees_20190910.txz"
system(paste("tar -Jxf", file.path("data-raw", txz_file), "-C /tmp"))
Dir <- paste0("/tmp/", sub("\\.txz$", "", txz_file))
Files <- list.files(Dir)
DB <- sub("_result_filtered.csv$", "", sub("^pnr74_", "", Files))
Files <- setNames(Files, DB)
for (db in DB) {
f <- file.path(Dir, Files[db])
  assign(db, fread(f, sep = "|", encoding = "Latin-1"))
}
system(paste("rm -r", Dir))
rm(db, Dir, f, Files, txz_file)

# Duplicated rows (on all variables) per database
dup <- mclapply(mc.cores = detectCores(), setNames(DB, DB), function(db) {
  dup <- unique(get(db)[, .N, keyby = names(get(db))][N > 1])
  dup$N <- dup$N - 1
  names(dup)[names(dup) == "N"] <- "Ndup"
  dup[order(dup$Ndup, decreasing = T)]
})
dup <- dup[sapply(dup, nrow) >  0]
n_dup <- do.call(rbind, lapply(dup, function(d) {
  c(Number_of_rows_which_are_duplicated = nrow(d),
    Number_of_duplicated_rows = sum(d$Ndup))
}))
p <- n_dup[, "Number_of_duplicated_rows"] / 
  sapply(rownames(n_dup), function(db) nrow(get(db)))
n_dup <- cbind(n_dup, Proportion_of_duplicated_rows = p)
rm(p)

# Check if two variables are equivalent
equ <- function(db, u, v) {
  J <- c(u, v)
  d <- unique(db[, ..J])
  d <- as.data.frame(table(d[[u]], d[[v]], useNA = "ifany"))
  d <- d[d$Freq > 0, ]
  d$Freq <- NULL
  r <- !(any(duplicated(d$Var1)) | any(duplicated(d$Var2)))
  if (!r) {
    r <- rbind(
      d[d$Var1 %in% d$Var1[duplicated(d$Var1)], ],
      d[d$Var2 %in% d$Var2[duplicated(d$Var2)], ]
    )
    names(r) <- c(u, v)
  }
  return(r)
}
equ(chop, "chop_NUMERO_SEQUENCE_CHOP", "chop_NUMERO_SEQUENCE_INTERVENTION")
equ(cim10, "cim10_CODE_CIM10", "cim10_CODE_CIM10_SANSPOINT")
equ(cim10, "cim10_CODE_CIM10", "cim10_LIB_CIM10")
head(equ(cim10, "cim10_CODE_CIM10", "cim10_TYPE_CODE_CIM10"))
equ(molis, "molis_CODE_TEST_MOLIS", "molis_LIB_TEST_MOLIS")
equ(molis_smoker, "molis_smoker_CODE_TEST_MOLIS",
    "molis_smoker_LIB_TEST_MOLIS")
equ(soarian_sg_occ, "soa_sg_occ_ASSMT_OBSV_ID", "soa_sg_occ_FND_LASTCNGDTIME")
head(equ(soarian_med, "medication_DOSE_PRESCRITE",
         "medication_VOLUME_PRESCRIT"))

# Pseudo duplicates
pdup <- function(db, J1, J2) {
  J0 <- c("p_IPP", "sej_NUMERO_SEJOUR")
  J3 <- names(db) %>%
    {.[!grepl("^(p|cg|sej)_", .)]} %>%
    {.[!(. %in% c(J1, J2))]}
  J <- c(J0, J1, J2, J3)
  db <- unique(db)
  tmp <- db[, .(count = .N), by = c(J0, J1)][count > 1][, count := NULL]
  merge(db[, ..J, with = F], tmp, by = c(J0, J1))
}
##
molis_J1 <- c("molis_NUMERO_BON", "molis_CODE_TEST_MOLIS",
              "molis_DATE_RECEPTION")
molis_J2 <- "molis_DATE_PRELEVEMENT"
molis_pdup <- pdup(molis, molis_J1, molis_J2)
##
predimed_J1 <- paste0(
  "medication_", c("ID_PRESCR", "MEDIC_NOM", "DATE_ADMIN", "DATE_PLANIF"))
predimed_J2 <- "medication_Durée effective du soluté"
predimed_pdup <- pdup(predimed, predimed_J1, predimed_J2)
##
soarian_med_J1 <- predimed_J1
soarian_med_J2 <- c()
soarian_med_pdup <- pdup(soarian_med, soarian_med_J1, soarian_med_J2)
##
chop_J1 <- c()
chop_J2 <- "chop_NUMERO_SEQUENCE_CHOP"
( chop_pdup <- pdup(chop, chop_J1, chop_J2) )
##
cim10_J1 <- "cim10_SEQUENCE_CODE_CIM10"
cim10_J2 <- "cim10_CODE_CIM10"
( cim10_pdup <- pdup(cim10, cim10_J1, cim10_J2) )
##
soarian_J1 <- c("soarian_Date effet formulaire", "soarian_Finding",
                "soarian_Type formulaire")
soarian_J2 <- "soarian_Valeur"
( soarian_pdup <- pdup(soarian, soarian_J1, soarian_J2) )

# Export
write_xlsx(path = "results/duplicates_20191022.xlsx", x = list(
  `soarian (true duplicates)` = dup$soarian,
  `molis (true duplicates)` = dup$molis,
  `molis (pseudo duplicates)` = molis_pdup,
  `predimed (pseudo duplicates)` = predimed_pdup,
  `soarian_med (pseudo duplicates)` = soarian_med_pdup
))
