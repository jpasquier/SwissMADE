# Libraries
library(jsonlite)
library(readxl)

# Prescriptions - List of all the files in the `Prescription` folder
files <- list.files("Prescription")

# Prescriptions - Read each file with the function `fromJSON`
prescr_files <- lapply(files, function(f) {
  id <- as.integer(sub("\\.deid\\.json$", "", sub("^prescriptions_", "", f)))
  out <- fromJSON(file.path("Prescription", f))
  out$id <- id
  out
})

# Prescriptions - Build the tables
prescr_tbls <- c("prescriptions", "acts")
prescr_tbls <- setNames(prescr_tbls, prescr_tbls)
prescr_tbls <- lapply(prescr_tbls, function(tbl) {
  do.call(rbind, lapply(prescr_files, function(z) {
    if (is.list(z[[tbl]]) & length(z[[tbl]]) == 0) {
      out <- NULL
    } else {
      out <- cbind(prescription_file = z$id, z[[tbl]])
    }
    return(out)
  }))
})

# Other tables (eds, labo, ofs)
other_tbls <- list(
  eds = read.delim("eds.csv"),
  labo = read.delim("labo.csv"),
  ofs = as.data.frame(read_xlsx("OFS.xlsx", guess_max = 10^5))
)

# Append lists
tbls <- c(prescr_tbls, other_tbls)
rm(prescr_tbls, other_tbls)
gc()

# Recoding
tbls <- lapply(tbls, function(tbl) {
  # Date format
  X <- grep("_date$", names(tbl), value = TRUE)
  X <- c(X, grep("^aborted_datetime$", names(tbl), value = TRUE))
  for (x in X) {
    tbl[[x]][!is.na(tbl[[x]]) & tbl[[x]] == ""] <- NA
    f1 <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}"
    f2 <- "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
    if (all(is.na(tbl[[x]]) | grepl(f1, tbl[[x]]))) {
      fmt = "%Y-%m-%dT%H:%M:%S"
    } else if (all(is.na(tbl[[x]]) | grepl(f2, tbl[[x]]))) {
      fmt = "%Y-%m-%d %H:%M:%S"
    } else {
      stop("check date format")
    }
    miss <- is.na(tbl[[x]])
    tbl[[x]] <- as.POSIXct(tbl[[x]], format = fmt)
    if (any(miss != is.na(tbl[[x]]))) {
      stop(paste("error in the conversion of", x, "from table", tbl))
    }
  }
  # Numeric format
  X <- sapply(tbl, function(x) class(x)[1] == "character")
  X <- names(tbl)[X]
  for(x in X) {
    z <- tbl[[x]]
    if (all(is.na(z) | grepl("^[0-9]+$", z))) {
      miss <- is.na(z)
      tbl[[x]] <- as.integer(z)
      if (any(miss != is.na(tbl[[x]]))) {
        stop(paste("error in the conversion of", x, "from table", tbl))
      }
    } else if (all(is.na(z) | grepl("^([0-9]+)?(\\.[0-9]+)?$", z))) {
      miss <- is.na(z)
      tbl[[x]] <- as.numeric(z)
      if (any(miss != is.na(tbl[[x]]))) {
        stop(paste("error in the conversion of", x, "from table", tbl))
      }
    }
  }
  return(tbl)
})


# Processing of prescriptions$acts
tbls$prescriptions$acts <- sapply(tbls$prescriptions$acts, function(z) {
  m <- "error in pprescriptions$acts processing"
  if (is.list(z)) {
    if(length(z) == 0) {
      r <- NA
    } else if (length(z) == 1) {
      r <- z[[1]]
    } else {
      stop(m)
    }
  } else if (is.vector(z)) {
    if (length(unique(z) == 1)) {
      r <- unique(z)
    } else {
      stop(m)
    }
  } else {
    stop(m)
  }
  return(r)
})

# Save data
for (x in names(tbls)) assign(x, tbls[[x]])
rm(x, tbls)
save(prescriptions, file = "prescriptions.rda", compress = "xz")
save(acts, file = "acts.rda", compress = "xz")
save(eds, file = "eds.rda", compress = "xz")
save(labo, file = "labo.rda", compress = "xz")
save(ofs, file = "ofs.rda", compress = "xz")
sink("import_hug_data_2_sessionInfo_20201209.txt")
print(sessionInfo(), locale = FALSE)
sink()
