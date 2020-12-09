library(jsonlite)

# Read the lines of the json file
print("Read the lines of the json file")
out <- readLines("patient_values.json")

# The line 746086 has to be modified to be read by the function fromJSON
out[746086] <- sub('"value": NaN', '"value": "NaN"', out[746086])

# Convert the lines
print("Convert the lines")
out <- lapply(out, fromJSON)

# Save temp file
print("Save temp file")
saveRDS(out, file = "patient_values_temp1.rds")


# Check the basic structure
print("Check the basic structure")
if (any(sapply(out, length) != 5)) stop("different lengths")
X <- c("_id", "begin_date", "patient_id", "end_date", "pv")
if (any(sapply(out, function(x) any(names(x) != X)))) stop("diferent names")
if (any(sapply(out, function(z) any(sapply(z[1:4], is.list))))) {
  stop("problem with the structure")
}
if (any(sapply(out, function(z) !all(sapply(z[1:4], length) == 1)))) {
  stop("problem with the structure")
}
rm(X)

# Add ids
print("Add ids")
out <- lapply(1:length(out), function(i) c(entry_id = i, out[[i]]))

# Check that datas are in each pv
print("Check that datas are in each pv")
if (any(sapply(out, function(z) all(names(z$pv) != "datas")))) {
  stop("pv without datas")
}

# Format pv$datas
print("Format pv$datas")
out <- lapply(out, function(z) {
  m <- "error in datas processing"
  if (is.list(z$pv$datas)) {
    if (length(z$pv$datas) == 0) {
      z$pv$datas <- NULL
    } else if (is.data.frame(z$pv$datas)) {
      V <- names(z$pv$datas)
      X <- c("subkey", "value_str", "value", "unit")
      if (all(V %in% X)) {
        if (any(!(X %in% V))) {
          for (x in X[!(X %in% V)]) {
            z$pv$datas[[x]] <- NA
          }
        }
      } else {
        stop(m)
      }
      z$pv$datas <- z$pv$datas[X]
    } else {
      stop(m)
    }
  } else {
    stop(m)
  }
  return(z)
})

# Extract datas
# pv$datas is the only object with multiple entries
# key: entry_id
print("Extract datas")
patient_values.pv.datas <- do.call(rbind, lapply(out, function(z) {
  d <- z$pv$datas
  if (is.null(d)) {
    r <- NULL
  } else {
    names(d) <- paste0("pv.datas.", names(d))
    r <- cbind(entry_id = z$entry_id, d)
  }
  return(r)
}))
out <- lapply(out, function(z) {
  z$pv <- z$pv[names(z$pv) != "datas"]
  return(z)
})

# Save pv$datas
print("Save pv$datas")
save(patient_values.pv.datas, file = "patient_values.pv.datas.rda",
     compress = "xz")
rm(patient_values.pv.datas)

# Transform pv$properties in a 1x2 data.frame
print("Transform pv$properties in a 1x2 data.frame")
out <- lapply(out, function(z) {
  x <- z$pv$properties
  p <- data.frame(DEVICE_SID = NA, TRIAGE_ID = NA)
  m <- "error in properties" 
  if (is.null(x)) {
    z$pv$properties <- p
  } else if (is.list(x)) {
    if (length(x) == 0) {
      z$pv$properties <- p
    } else if (length(x)  == 1) {
      if (names(x) == "DEVICE_SID") {
        if (is.vector(x$DEVICE_SID)) {
          if (length(x$DEVICE_SID) == 1) {
            p$DEVICE_SID <- x$DEVICE_SID
            z$pv$properties <- p
          } else {
            stop(m)
          }
        } else {
          stop(m)
        }
      } else if (names(x) == "TRIAGE_ID") {
        if (is.vector(x$TRIAGE_ID)) {
          if (length(x$TRIAGE_ID) == 1) {
            p$TRIAGE_ID <- x$TRIAGE_ID
            z$pv$properties <- p
          } else {
            stop(m)
          }
        } else {
          stop(m)
        }
      } else {
        stop(m)
      }
    } else {
      stop(m)
    }
  } else {
    stop(m)
  }
  return(z)
})

# Check that all elements of pv are of length 1
# properties is a 1x2 data.frame
print("Check that all elements of pv are of length 1")
b <- sapply(out, function(x) {
  z <- x$pv
  z <- z[names(z) != "properties"]
  if (any(sapply(z, is.list))) {
    r <- TRUE
  } else if (any(sapply(z, length) != 1)) {
    r <- TRUE
  } else {
    r <- FALSE
  }
  r
})
if (any(b)) stop("problem with the structure of pv")
rm(b)

# Add missing objects to pv
print("Add missing objects to pv")
X <- unique(do.call(base::c, lapply(out, function(z) names(z$pv))))
out <- lapply(out, function(z) {
  V <- names(z$pv)
  if (any(!(X %in% V))) {
    for (x in X[!(X %in% V)]) {
      z$pv[[x]] <- NA
    }
  }
  return(z)
})
rm(X)

# Save temp file
print("Save temp file")
saveRDS(out, file = "patient_values_temp2.rds")

# Convert each element to a dataframe (1 row)
# It must be done by piece because otherwise it takes too long
print("Convert each element to a dataframe (1 row)")
if (!dir.exists("tmp_files")) {
  dir.create("tmp_files")
}
k <- 500
n <- length(out)
m <- ceiling(n / k)
for (l in 1:k) {
  print(l)
  j <- ((l - 1) * m + 1):min(l * m, n)
  tmp <- do.call(rbind, lapply(out[j], function(z) {
    z$pv <- do.call(cbind, z$pv)
    do.call(cbind, z)
  }))
  tmp_file <- paste0("tmp_files/pv_tmp_", l, ".rds")
  saveRDS(tmp, tmp_file)
  rm(tmp, tmp_file)
  gc()
}
rm(j, k, l, m, n)

# Append all dataframes
print("Append all dataframes")
i <- 1
f_list <- list.files("tmp_files")
patient_values <- NULL
for (f in f_list) {
  print(i)
  i <- i + 1
  t0 <- Sys.time()
  patient_values <- rbind(patient_values, readRDS(file.path("tmp_files", f)))
  print(Sys.time() - t0)
}
rm(f, f_list, i, t0)

# Save data
print("Save data")
save(patient_values, file = "patient_values.rda", compress = "xz")
sink("import_hug_data_1_sessionInfo_20201209.txt")
print(sessionInfo(), locale = FALSE)
sink()
