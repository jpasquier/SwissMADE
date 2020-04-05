# Libraries
library(data.table)
library(writexl)

# DT threads
setDTthreads(0)

# Working directory
setwd("~/Projects/SwissMADE")

# Raw data directory
Dir01 <- "data-raw/PNR74_extractions_donnees_structurees_20190910"
Dir02 <- "data-raw/PNR74_extractions_donnees_structurees_20191112"

# Data directory
Dir1 <- "data/preprocessed_20191219"
if(!file.exists(Dir1)) dir.create(Dir1)

# Log file
log_file <- "data-raw/preprocessing_20191219.txt"
file.create(log_file)

# Load databases and store them as RData files
DB <- c("chop", "cim10", "molis", "molis_smoker", "mouvement", "ofs",
        "predimed", "soarian_frcv", "soarian_med", "soarian", "soarian_sg_occ")
for (db in DB) {
  f0 <- paste0("pnr74_", db, "_result_filtered.csv.xz")
  if (db == "molis") {
    f0 <- file.path(Dir02, f0)
  } else {
    f0 <- file.path(Dir01, f0)
  }
  assign(db, fread(cmd = paste("unxz -cq", f0),
                   sep = "|", encoding = "Latin-1"))
}
rm(Dir01, Dir02, db, f0)

# Remove duplicates
sink(log_file, append = TRUE)
cat("Duplicated lines are removed\n")
cat("============================\n\n")
for (db in DB) {
  n0 <- nrow(get(db))
  assign(db, unique(get(db)))
  n <- nrow(get(db))
  if (n0 - n > 0) {
    cat(paste0(db, ": ", n0 - n, " duplicates were removed\n"))
  } else {
    cat(paste0(db, ": no duplicate\n"))
  }
}
cat("\n")
rm(db, n0, n)
gc()
sink()

# Pseudo duplicates
sink(log_file, append = TRUE)
cat("Pseudo duplicates\n=================\n\n")
cat(paste(
  "molis case 1\n------------\n",
  "p_IPP 83984 sej_NUMERO_SEJOUR 316027846 molis_NUMERO_BON 1011341540\n",
  "molis_CODE_TEST_MOLIS 'PREL':\n",
  "Two observations which only differs on molis_RESULTAT_TEXTE\n",
  "First observation: 'URINE PAR SONDE A DEMEURE (URISWAB)'\n",
  "Second observation: 'URINE PAR SONDE A DEMEURE'\n",
  "Only the first observation is kept\n\n"
))
n <- nrow(molis)
molis <- molis[!(
  p_IPP == 83984 & sej_NUMERO_SEJOUR == 316027846 &
    molis_NUMERO_BON == 1011341540 & molis_CODE_TEST_MOLIS == "PREL" &
    molis_RESULTAT_TEXTE == "URINE PAR SONDE A DEMEURE"), ]
if (n - nrow(molis) != 1) stop()
rm(n)
cat(paste(
  "molis case 2\n------------\n",
  "p_IPP 509780 sej_NUMERO_SEJOUR 315051498 molis_NUMERO_BON 3517596455\n",
  "molis_CODE_TEST_MOLIS 'PTT':\n",
  "Two observations which only differs on molis_DATE_PRELEVEMENT\n",
  "First observation: '12.07.2015 00:00:00'\n",
  "Second observation: '12.07.2015 12:30:00'\n",
  "Only the second observation is kept\n\n"
))
n <- nrow(molis)
molis <- molis[!(
  p_IPP == 509780 & sej_NUMERO_SEJOUR == 315051498 &
    molis_NUMERO_BON == 3517596455 & molis_CODE_TEST_MOLIS == "PTT" &
    molis_DATE_PRELEVEMENT == "12.07.2015 00:00:00"), ]
if (n - nrow(molis) != 1) stop()
rm(n)
cat(paste(
  "molis case 3\n------------\n",
  "p_IPP 943592 sej_NUMERO_SEJOUR 316066019 molis_NUMERO_BON 316066019 \n",
  "molis_DATE_RECEPTION == '23.07.2016 21:19:40':\n",
  "This is the only case for which codes are duplicated for the same\n",
  "return date (molis_DATE_RECEPTION). Half of the tests involve a venous\n",
  "line and the other half an arterial line. We add a second to the return\n",
  "date of the venous line tests to distinguish them from the arterial\n",
  "line and not to add a new identifying variable for this unique case.\n\n"
))
n <- nrow(molis)
molis[p_IPP == 943592 & sej_NUMERO_SEJOUR == 316066019 &
        molis_NUMERO_BON == 316066019 &
        molis_DATE_RECEPTION == "23.07.2016 21:19:40" &
        molis_DATE_PRELEVEMENT == "23.07.2016 21:18:00",
      "molis_DETAIL_RESULTAT_PROPRE"] <- "Veineux"
molis[p_IPP == 943592 & sej_NUMERO_SEJOUR == 316066019 &
        molis_NUMERO_BON == 316066019 &
        molis_DATE_RECEPTION == "23.07.2016 21:19:40" &
        molis_DATE_PRELEVEMENT == "23.07.2016 21:18:00",
      "molis_DATE_RECEPTION"] <- "23.07.2016 21:19:41"
molis[p_IPP == 943592 & sej_NUMERO_SEJOUR == 316066019 &
        molis_NUMERO_BON == 316066019 &
        molis_DATE_RECEPTION == "23.07.2016 21:19:40" &
        molis_DATE_PRELEVEMENT == "23.07.2016 21:17:00",
      "molis_DETAIL_RESULTAT_PROPRE"] <- "Artériel"
if (n - nrow(molis) != 0) stop()
rm(n)
cat(paste(
  "predimed\n--------\n",
  "p_IPP 3000320 sej_NUMERO_SEJOUR 509780 medication_ID_PRESCR 368192:\n",
  "Two observations which only differs on\n",
  "medication_Durée effective du soluté and\n",
  "medication_Volume effectif du soluté\n",
  "First observation: '24 h' / 4000\n",
  "Second observation: '0 h' / 0\n",
  "Only the first observation is kept\n\n"
))
n <- nrow(predimed)
predimed <- predimed[!(
  p_IPP == 3000320 & sej_NUMERO_SEJOUR == 315100210 &
    medication_ID_PRESCR == 368192 &
    medication_MEDIC_NOM == "SODIUM BICARBONATE sol perf 8.4 %" &
    medication_DATE_PLANIF == "24.10.2015 15:10" &
    `medication_Durée effective du soluté` == "0 h"), ]
if (n - nrow(predimed) != 1) stop()
rm(n)
cat(paste(
  "soarian_med\n-----------\n",
  "For some observations, there are several lines for the same\n",
  "administration date. We can distinguish two cases:\n",
  "  1) The lines differ on the variables medication_DOSE_ADMIN and\n",
  "     medication_DOSE_ADMIN_UNITE and possibly on the variables\n",
  "     medication_DOSE_CONVERSION, medication_DOSE_PRESCRITE,\n",
  "     medication_FREQUENCE and medication_VOLUME_PRESCRIT. In these\n",
  "     cases, there is exactly one line for which the variables\n",
  "     medication_DOSE_ADMIN and medication_DOSE_ADMIN_UNITE are\n",
  "     filled. For each case, only this line is kept, the others\n",
  "     are deleted.\n",
  "  2) The lines differ on the variables medication_DOSE_CONVERSION,\n",
  "     medication_DOSE_PRESCRITE and medication_VOLUME_PRESCRIT. In\n",
  "     these cases medication_DOSE_ADMIN and medication_DOSE_ADMIN_UNITE\n",
  "     are empty and only the first line is kept. The choice of the\n",
  "     first line is arbitrary.\n"
))
n0 <- nrow(soarian_med)
J <- c("p_IPP", "sej_NUMERO_SEJOUR", paste0(
  "medication_", c("ID_PRESCR", "MEDIC_NOM", "DATE_PLANIF", "DATE_ADMIN")))
tmp <- soarian_med[, .(count = .N), by = J][count > 1]
n <- nrow(tmp)
npdup <- sum(tmp$count - 1)
tmp <- tmp[, count := NULL][, pdup_id := seq_len(.N)]
soarian_med <- merge(soarian_med, tmp, by = J, all = TRUE)
soarian_med[!is.na(pdup_id), pdup_n := seq_len(.N), by = pdup_id]
dv <- function(DF) {
  J <- sapply(DF, function(x) length(unique(x)) > 1)
  paste(sort(names(DF)[J]), collapse = ";")
}
soarian_med[!is.na(pdup_id), diff_var := dv(.SD), by = pdup_id]
tmp <- soarian_med[!is.na(diff_var) & grepl("DOSE_ADMIN", diff_var),
                   .(count = sum(medication_DOSE_ADMIN != "" |
                       medication_DOSE_ADMIN_UNITE != "")),
                   by = pdup_id]
if (!all(tmp$count == 1)) stop()
tmp <- soarian_med[!is.na(diff_var) & !grepl("DOSE_ADMIN", diff_var),
                   c("medication_DOSE_ADMIN", "medication_DOSE_ADMIN_UNITE")]
if (!all(tmp == "")) stop()
soarian_med <- soarian_med[is.na(diff_var) | !grepl("DOSE_ADMIN", diff_var) |
                             medication_DOSE_ADMIN != "" |
                             medication_DOSE_ADMIN_UNITE != "", ]
soarian_med <- soarian_med[is.na(diff_var) | grepl("DOSE_ADMIN", diff_var) |
                             pdup_n == 1, ]
n1 <- nrow(soarian_med)
b <- sum(!is.na(soarian_med$pdup_id)) == n &
       n0 - n1 == npdup &
       all(soarian_med[!is.na(pdup_id), .(test = .N == 1), by = pdup_id]$test)
if(!b) stop()
cat(paste0(
  "Number of duplicated observations: ", n, "\n",
  "Number of removed duplicates: ", npdup, "\n\n"
))
soarian_med[, pdup_id := NULL]
soarian_med[, pdup_n := NULL]
soarian_med[, diff_var := NULL]
rm(n0, J, tmp, n, npdup, dv, n1, b)
sink()

# Type casting
type_casting <- function(db) {
  conv <- NULL
  for (x in names(db)) {
    if (class(db[[x]]) == "numeric") {
      if (all((db[[x]] >= 0 & db[[x]] %% 1 == 0) | is.na(db[[x]]))) {
        if (all(db[[x]] <= .Machine$integer.max | is.na(db[[x]]))) {
          db[, (x) := as.integer(get(x))]
          conv <- rbind(conv, data.frame(
            variable = x, original_type = "numeric", new_type = "integer"))
        } else {
          db[, (x) := bit64::as.integer64(get(x))]
          conv <- rbind(conv, data.frame(
            variable = x, original_type = "numeric", new_type = "integer64"))
        }
      }
    } else if (class(db[[x]]) == "character") {
      dpat <- "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}$"
      tpat1 <- "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$"
      tpat2 <- "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4} [0-9]{2}:[0-9]{2}$"
      if (all(grepl(dpat, db[[x]]) | db[[x]] == "" | is.na(db[[x]]))) {
        db[, (x) := as.Date(get(x), format = "%d.%m.%Y")]
        conv <- rbind(conv, data.frame(
            variable = x, original_type = "character", new_type = "Date"))
      } else if (all(grepl(tpat1, db[[x]]) | db[[x]] == "" | is.na(db[[x]]))) {
        db[, (x) := as.POSIXct(get(x), format = "%d.%m.%Y %H:%M:%S",
                               tz = "Europe/Zurich")]
        conv <- rbind(conv, data.frame(
            variable = x, original_type = "character", new_type = "POSIXct"))
      } else if (all(grepl(tpat2, db[[x]]) | db[[x]] == "" | is.na(db[[x]]))) {
        db[, (x) := as.POSIXct(get(x), format = "%d.%m.%Y %H:%M",
                               tz = "Europe/Zurich")]
        conv <- rbind(conv, data.frame(
            variable = x, original_type = "character", new_type = "POSIXct"))
      }
    } else if (x == "sej_NUMERO_SUPER_SEJOUR" & class(db[[x]]) == "logical") {
      if (all(is.na(db[[x]]))) {
        db[, (x) := as.integer(get(x))]
      }
    }
  }
  if (!is.null(conv)) {
    setattr(db, "conv", conv)
  }
}
sink(log_file, append = TRUE)
cat("Type casting\n============\n\n")
options(warn = 2)
for(db in DB) {
  type_casting(get(db))
  cat(paste0(db, "\n", paste(rep("-", nchar(db)), collapse = ""), "\n"))
  print(attr(get(db), "conv"), row.names = FALSE)
  cat("\n")
}
rm(db)
sink()

# Variable types - incompatibilities
Merge <- function(x, y) merge (x, y, by = "variable", all = TRUE, sort = FALSE)
var_type_tbl <- Reduce(Merge, lapply(DB, function(db) {
  U <- sapply(get(db), function(x) {
    u <- class(x)
    if (length(u) == 2 & u[1] == "POSIXct" & u[2] == "POSIXt") {
      u <- "POSIXct"
    } else if (length(u) > 1) {
      u <- paste(u, collapse = ";")
    }
    return(u)
  })
  U <- data.frame(variable = names(U), type = U)
  rownames(U) <- NULL
  colnames(U)[2] <- db
  return(U)
}))
b <- apply(var_type_tbl[, -1], 1, function(x) length(unique(na.omit(x))) == 1)
sink(log_file, append = TRUE)
cat("Variable types: incompatibilities\n=================================\n\n")
print(var_type_tbl[!b, !apply(is.na(var_type_tbl[!b, ]), 2, all)],
      row.names = FALSE)
cat("\n")
cat("predimed\n--------\n\n")
print(with(predimed, table(medication_TYPE_PRESCRIPTION)))
cat("\n")
print(with(predimed, table(medication_EN_RESERVE)))
cat("\n")
cat("soarian_med\n-----------\n\n")
print(with(soarian_med, table(medication_TYPE_PRESCRIPTION)))
cat("\n")
print(with(soarian_med, table(medication_EN_RESERVE)))
cat("\n\n")
sink()
rm(b, Merge)

# Save databases
for (db in DB) {
  f1 <- file.path(Dir1, paste0(db, ".rda"))
  save(list = db, file = f1, version = 2, compress = "xz")
  rm(list = db)
}
rm(Dir1, db, f1)

# Save file type table
write_xlsx(list(variable_type = var_type_tbl),
           "data-raw/preprocessing_vartype_20191219.xlsx")

# Add session infos to log file
sink(log_file, append = TRUE)
cat("Session infos\n=============\n\n")
print(sessionInfo(), locale = FALSE)
sink()
