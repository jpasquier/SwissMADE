# Librairies
pkgs <- c("tcltk", "data.table", "parallel", "magrittr", "readxl", "writexl")
npkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(npkgs)) install.packages(npkgs)
for (pkg in pkgs) library(pkg, character.only = TRUE)
rm(pkg, pkgs, npkgs)

# DT threads
setDTthreads(0)

# Determine the number of cores according to the OS
if (Sys.info()[["sysname"]] == "Linux") {
  ncores <- detectCores()
} else if (Sys.info()[["sysname"]] == "Windows") {
  ncores <- 1
} else {
  stop()
}

# Working directory
wd <- "~/Projects/SwissMADE"
if(!file.exists(wd)) wd <- tk_choose.dir(caption = "Select working directory")
setwd(wd)
rm(wd)

# raw data directory
Dir <- "data/PNR74_extractions_donnees_structurees_20190910"
if(!file.exists(Dir)) Dir <- tk_choose.dir(caption = "Select data directory")

# Load databases
for (db in c("chop", "cim10", "ofs")) {
  f0 <- paste0(db, ".rda")
  f <- file.path(getwd(), Dir, f0)
  if(!file.exists(f)) stop(paste("cannot find", f0, "in data directory"))
  load(f)
}
rm(Dir, db, f0, f)

# Read codes from the xlsx file
# sheets: list containing each sheet of the Excel file
f0 <- "Code_Lists_ETEV_Hemo_ETEA_20191106.xlsx"
f <- file.path("documents", f0)
if(!file.exists(f)) f <- tk_choose.files(caption = paste("Select", f0))
if (length(f) == 0) stop("No file selected")
if (length(f) > 1) stop("More than one file selected")
sheets <- excel_sheets(f) %>%
  {setNames(., .)} %>%
  lapply(function(z) read_xlsx(f, z))
rm(f0, f)

# Remove return carriage codes (\r\n) in the headers
sheets <- lapply(sheets, function(sheet) {
  names(sheet) <- sub("\\r\\n", " ", names(sheet))
  return(sheet)
})

# Rename `Type of Life-threatening Haemorr`
names(sheets$Life_threatening_Hemo_CIM10GM)[
  names(sheets$Life_threatening_Hemo_CIM10GM) ==
    "Type of Life-threatening Haemorr"] <- "Type_LTH"

# Aggregate diagnostic and procedure codes from the xlsx file
code_tbl <- mclapply(mc.cores = ncores, 1:2, function(k) {
  # Define supplementary colums for each xlsx sheet
  supp_col <- list(
    Haemorrage_ICD10GM = c(sc1 = "Acronym1", sc2 = "Acronym2"),
    Sev_Haemorrhage_ICD10GM = c(sc1 = "Acronym1", sc2 = "Acronym2"),
    Ttt_Haemo_CHOP = c(sc1 = "TTT_Proc_class_Hemo"),
    Life_threatening_Hemo_CIM10GM = c(sc3 = "Type_LTH"),
    Blood_RedCellC_CHOP = c(),
    DVT_CIM10GM = c(sc1 = "VTE_Type", sc2 = "VTE_location"),
    Sens_Anal_DVT_CIM10GM = c(sc1 = "VTE_Type", sc2 = "VTE_location"),
    PE_CIM10GM = c(),
    Dx_DVT_PE_CHOP = c(sc1 = "DX_Proc_class_VTE"),
    Ttt_DVT_PE_CHOP = c(sc1 = "TTT_Proc_class_VTE"),
    Acute_Myoc_Infarct = c(),
    Stroke = c(),
    TIA = c(),
    Systemic_embolism = c()
  )
  # Append sheets with diagnostic/procedure codes 
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
    # Check if the data frame contains diagnostic or procedure variables
    if (!all(c(code, "Code racine", sel.col) %in% names(df))) {
      return(NULL)
    }
    # Select diagnostic code
    b <- df[sel.col] %>% {!is.na(.) & . == 1} %>% apply(1, any)
    # Select lines and columns
    sc <- supp_col[[sheet.name]]
    df <- df[b, c(code, "Code racine", descr.col, sc)]
    # Convert `Code racine` variable to boolean
    if (any(!(df[["Code racine"]] %in% c("code racine", "", NA)))) stop()
    df[["Code racine"]] <- df[["Code racine"]] %in% "code racine"
    # Rename descr.col
    names(df)[names(df) == "Code racine"] <- "code_racine"
    # Rename descr.col
    names(df)[names(df) == descr.col] <- "description"
    # Rename supplementary columns (supp_col_[1-3]) and store the previous name
    # in a second variable (supp_col_[1-3]_name)
    for (k in 1:3) {
      j <- paste0("sc", k)
      if (j %in% names(sc)) {
        names(df)[names(df) == sc[j]] <- paste0("supp_col_", k)
        df[[paste0("supp_col_", k, "_name")]] <- sc[j]
      } else {
        df[[paste0("supp_col_", k)]] <- NA
        df[[paste0("supp_col_", k, "_name")]] <- NA
      }
    }
    # Return selected diagnostics/procedures
    return(cbind(xlsx_sheet = sheet.name, df, stringsAsFactors = FALSE))
  }))
})
names(code_tbl) <- c("diag_code_tbl", "proc_code_tbl")

# Check if all sheets were parsed
names(sheets) %>% .[!(. %in% c(code_tbl[[1]][[1]], code_tbl[[2]][[1]]))]

# Some codes appears in multiple sheets -> aggregate them
code_tbl <- mclapply(mc.cores = ncores, code_tbl, function(tbl) {
  code <- grep("ICD10GM2_code2|Code_CHOP2", names(tbl), value = TRUE)
  # check if some variables are not unequivocal among sheet for a given code
  for (v in c("code_racine", paste0("supp_col_", 1:3))) {
    b <- aggregate(as.formula(paste(v, "~", code)), tbl,
                   function(z) length(unique(na.omit(z))) >  1,
                   na.action = NULL)[, 2]
    if(any(b)) stop(paste(v, "is not unequivocal"))
  }
  # aggregate xlsx sheet names and description
  code_racine <- unique(tbl[c(code, "code_racine")])
  Merge <- function(x, y) merge(x, y, by = code, all = T)
  V <- c("xlsx_sheet", "description",
         as.vector(t(outer(paste0("supp_col_", 1:3),
                           c("", "_name"), paste0))))
  tbl <- Reduce(Merge, lapply(V, function(v) {
    aggregate(as.formula(paste(v, "~", code)), tbl, function(z) {
      paste(unique(na.omit(z)), collapse = " / ")
    }, na.action = NULL)
  }))
  Merge(code_racine, tbl)
})

# ofs - diagnostics / procedure
# compute frequency of each code
tbl <- mclapply(mc.cores = ncores, 1:2, function(k) {
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

# add code description to the frequency table
tbl <- mclapply(mc.cores = ncores, 1:2, function(k) {
  if (k == 1) {
    code <- "ICD10GM2_code2"
    type <- "diagnostic"
  } else {
    code <- "Code_CHOP2"
    type <- "procedure"
  }
  V <- c(code, "code_racine", "xlsx_sheet", "description",
         as.vector(t(outer(paste0("supp_col_", 1:3),
                           c("", "_name"), paste0))))
  tmp <- setNames(do.call(data.frame, as.list(rep(NA, length(V)))), V)
  tab <- cbind(tbl[[k]], tmp)
  tab <- tab[c(1, (ncol(tab) - length(V) + 1):ncol(tab),
               2:(ncol(tab) - length(V)))]
  ctbl <- code_tbl[[k]]
  for (i in 1:nrow(ctbl)) {
    pattern <- paste0("^", ctbl[i, code])
    if (!ctbl[i, "code_racine"]) pattern <- paste0(pattern, "$")
    b <- grepl(pattern, tab[[type]])
    for (v in V) tab[b, v] <- ctbl[i, v]
  }
  tab[order(as.numeric(!is.na(tab[[code]])), tab$total, decreasing = TRUE),
      sapply(tab, function(z) !all(is.na(z) | z == ""))]
})

# Export results and session info
if (!file.exists("results")) dir.create("results")
write_xlsx(tbl, "results/ofs_diag_proc_tbl_20191118.xlsx")
sink("results/sessionInfo_20191118.txt")
print(sessionInfo(), locale = FALSE)
sink()

