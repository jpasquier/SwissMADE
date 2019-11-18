# Librairies
library(data.table)
library(parallel)
library(magrittr)
library(readxl)
library(writexl)

# DT threads
setDTthreads(0)

# Working directory
setwd("~/Projects/SwissMADE")

# raw data directory
Dir <- "data-raw/PNR74_extractions_donnees_structurees_20190910"

# Load databases
for (db in c("chop", "cim10", "ofs")) {
  f <- file.path(getwd(), Dir, paste0("pnr74_", db, "_result_filtered.csv.xz"))
  assign(db, fread(cmd = paste("unxz -cq", f),
                   sep = "|", encoding = "Latin-1"))
}
rm(db, f)

# Read codes from the xlsx file
# sheets: list containing each sheet of the Excel file
path <- "documents/Code_Lists_ETEV_Hemo_ETEA_20191106.xlsx"
sheets <- excel_sheets(path) %>%
  {setNames(., .)} %>%
  lapply(function(z) read_xlsx(path, z))

# Remove return carriage codes (\r\n) in the headers
sheets <- lapply(sheets, function(sheet) {
  names(sheet) <- sub("\\r\\n", " ", names(sheet))
  return(sheet)
})

# Aggregate diagnostic and procedure codes from the xlsx file
code_tbl <- mclapply(mc.cores = detectCores(), 1:2, function(k) {
  do.call(rbind, lapply(names(sheets), function(sheet.name) {
    # Dataframe
    df <- sheets[[sheet.name]]
    # Variables
    if (k == 1) {
      code <- "ICD10GM2_code2"
      sel.col <- c("CIM-10-GM 2014: données 2015-2016",
                   "CIM-10-GM 2016: données 2017")
    } else {
      code <- "Code_CHOP2"
      sel.col <- c("CHOP 2016", "CHOP 2017")
    }
    descr.col <- grep("^Description -", names(df), value = TRUE)
    # Check if the data frame contains diagnostic variables
    if (!all(c(code, "Code racine", sel.col) %in% names(df))) {
      return(NULL)
    }
    # Select diagnostic code
    b <- df[sel.col] %>% {!is.na(.) & . == 1} %>% apply(1, any)
    # Select lines and columns
    df <- df[b, c(code, "Code racine", descr.col)]
    # Convert is.root variable to boolean
    if (any(!(df[["Code racine"]] %in% c("code racine", "", NA)))) stop()
    df[["Code racine"]] <- df[["Code racine"]] %in% "code racine"
    # Rename descr.col
    names(df)[names(df) == "Code racine"] <- "code_racine"
    # Rename descr.col
    names(df)[names(df) == descr.col] <- "description"
    # Return selected diagnostics
    return(cbind(xlsx_sheet = sheet.name, df, stringsAsFactors = FALSE))
  }))
})
names(code_tbl) <- c("diag_code_tbl", "proc_code_tbl")

# Check if all sheets were parsed
names(sheets) %>% .[!(. %in% c(code_tbl[[1]][[1]], code_tbl[[2]][[1]]))]

# Some codes appears in multiple sheets
code_tbl <- mclapply(mc.cores = detectCores(), code_tbl, function(tbl) {
  code <- grep("ICD10GM2_code2|Code_CHOP2", names(tbl), value = TRUE)
  # check if the root code status differs among sheet for a given code
  b <- aggregate(as.formula(paste("code_racine ~", code)), tbl,
                 function(z) length(unique(z)) == 0)[, 2]
  if(any(b)) stop("root code status is not unequivocal")
  root_code <- unique(tbl[c(code, "code_racine")])
  # aggregate xlsx sheet names and description
  Merge <- function(x, y) merge(x, y, by = code, all = T)
  tbl <- Reduce(Merge, lapply(c("xlsx_sheet", "description"), function(v) {
    aggregate(as.formula(paste(v, "~", code)), tbl, function(z) {
      paste(unique(z), collapse = " / ")
    })
  }))
  Merge(root_code, tbl)
})

# ofs - diagnostics / procedure
tbl <- mclapply(mc.cores = detectCores(), 1:2, function(k) {
  if (k == 1) {
    r <- ".*?4\\.2(\\.)?V([0-5][0-9]0).*"
    V <- grep(r, names(ofs), value = T) %>%
      {.[order(as.numeric(sub(r, "\\2", .)))]}
    u <- "diagnostic"
  } else {
    r <- ".*?4\\.3\\.V([0-9]{2}|100)0_.*"
    V <- grep(r, names(ofs), value = T) %>%
      {.[order(as.numeric(sub(r, "\\1", .)))]}
    u <- "procedure"
  }
  Merge <- function(x, y) merge(x, y, by = u, all = TRUE)
  tbl <- Reduce(Merge, lapply(V, function(v) {
    tbl <- table(ofs[[v]], useNA = "ifany") %>%
      as.data.frame() %>%
      setNames(c(u, v))
    tbl[[u]] <- as.character(tbl[[u]])
    tbl[is.na(tbl[[u]]), u] <- "<NA>"
    return(tbl)
  }))
  tbl[V][is.na(tbl[V])] <- 0
  tbl$total <- apply(tbl[V], 1, sum)
  tbl <- tbl[order(tbl$total, decreasing = TRUE), c(u, "total", V)]
  return(tbl)
})
tbl <- setNames(tbl, c("ofs_diag_tbl", "ofs_proc_tbl"))

head(tbl$ofs_diag_tbl)
head(code_tbl$diag_code_tbl)

# add description
tbl <- mclapply(mc.cores = detectCores(), 1:2, function(k) {
  if (k == 1) {
    code <- "ICD10GM2_code2"
    type <- "diagnostic"
  } else {
    code <- "Code_CHOP2"
    type <- "procedure"
  }
  tmp <- setNames(data.frame(NA, NA, NA, NA),
                  c(code, "code_racine", "xlsx_sheet", "description"))
  tab <- cbind(tbl[[k]], tmp)
  tab <- tab[c(1, (ncol(tab) - 3):ncol(tab), 2:(ncol(tab) - 4))]
  ctbl <- code_tbl[[k]]
  for (i in 1:nrow(ctbl)) {
    pattern <- paste0("^", ctbl[i, code])
    if (!ctbl[i, "code_racine"]) pattern <- paste0(pattern, "$")
    b <- grepl(pattern, tab[[type]])
    tab[b, code] <- ctbl[i, code]
    tab[b, "code_racine"] <- ctbl[i, "code_racine"]
    tab[b, "xlsx_sheet"] <- ctbl[i, "xlsx_sheet"]
    tab[b, "description"] <- ctbl[i, "description"]
  }
  tab[order(as.numeric(!is.na(tab[[code]])), tab$total, decreasing = TRUE), ]
})

write_xlsx(tbl, "results/ofs_diag_proc_tbl_20191113.xlsx")


