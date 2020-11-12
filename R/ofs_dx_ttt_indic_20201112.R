# Libraries
pkgs <- c("tcltk", "data.table", "parallel", "magrittr", "readxl")
npkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(npkgs)) install.packages(npkgs)
for (pkg in pkgs) library(pkg, character.only = TRUE)
rm(pkg, pkgs, npkgs)

# DT threads
setDTthreads(0)

# Working directory
wd <- "~/Projects/SwissMADE"
if(!file.exists(wd)) wd <- tk_choose.dir(caption = "Select working directory")
setwd(wd)
rm(wd)

# Data directory
data_dir <- "data"
if(!file.exists(data_dir)) {
  data_dir <- tk_choose.dir(caption = "Select data directory")
}

# Load ofs database
f0 <- "ofs.rda"
f <- file.path(getwd(), data_dir, f0)
if(!file.exists(f)) stop(paste("cannot find", f0, "in data directory"))
load(f)
rm(data_dir, f0, f)

# Read codes from the xlsx file
# sheets: list containing each sheet of the Excel file
f0 <- "Code_Lists_ETEV_Hemo_ETEA_20200228.xlsx"
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

# Remove a remaining dot in a code (Y57.9 in ADE_... tables)
for (u in c("ADE_ATE", "ADE_Hemo", "ADE_VTE")) {
  sheets[[u]]$ICD10GM2_code2 %<>% sub("\\.", "", .)
}
rm(u)

# Remove all M628 and M6289 codes (Source: mail MALP 03.11.2020)
sheets <- lapply(setNames(names(sheets), names(sheets)), function(sn) {
  sheet <- sheets[[sn]]
  if (any(names(sheet) == "ICD10GM2_code2")) {
    i1 <- sheet$ICD10GM2_code2 == "M628"
    i2 <- sheet$ICD10GM2_code2 == "M6289"
    sheet <- sheet[!i1 & !i2, ]
    if (sum(i1) > 0) {
      message(paste(sum(i1), "M628 code(s) was/were deleted from sheet", sn))
    }
    if (sum(i2) > 0) {
      message(paste(sum(i1), "M6289 code(s) was/were deleted from sheet", sn))
    }
  }
  return(sheet)
})

# Aggregate diagnostic and procedure codes from the xlsx file
code_tbl <- mclapply(1:2, function(k) {
  # Define supplementary colums for each xlsx sheet
  supp_col <- list(
    Hemo = c(sc1 = "Acronym1", sc2 = "Acronym2"),
    Hemo_Traum = c(sc1 = "Acronym1", sc2 = "Acronym2"),
    Sev_Hemo = c(sc1 = "Acronym1", sc2 = "Acronym2"),
    Transfu = c(),
    ADE_Hemo = c(sc1 = "Acronym1", sc2 = "Acronym2"),
    DVT = c(),
    PE = c(),
    ADE_VTE = c(sc1 = "Acronym1", sc2 = "Acronym2"),
    ATE = c(),
    AMI = c(),
    Stroke = c(),
    TIA = c(),
    Systemic_embolism = c(sc1 = "Acronym1"),
    ADE_ATE = c(sc1 = "Acronym1", sc2 = "Acronym2")
  )
  # Append sheets with diagnostic/procedure codes 
  tbl <- do.call(rbind, lapply(names(sheets), function(sheet.name) {
    # Dataframe
    df <- sheets[[sheet.name]]
    # Variables
    if (k == 1) {
      code <- "ICD10GM2_code2"
      sel.col <- c("CIM-10-GM 2014: données 2015-2016",
                   "CIM-10-GM 2016: données 2017")
    } else {
      code <- "Code_CHOP2"
      sel.col <- c("CHOP 2015", "CHOP 2016", "CHOP 2017")
    }
    descr.col <- grep("^Description -", names(df), value = TRUE)
    # Check if the data frame contains diagnostic or procedure variables
    if (!all(c(code, "Code racine", sel.col) %in% names(df))) {
      return(NULL)
    }
    # Check if dots are remaining in codes
    if (any(grepl("\\.", df[[code]]))) stop("dots are remaining in codes")
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
    # Rename supplementary columns (supp_col_[1-2]) and store the previous name
    # in a second variable (supp_col_[1-2]_name)
    for (k in 1:2) {
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
  # add simplified sheet names
  #   tbl$.i <- 1:nrow(tbl)
  #   tbl <- merge(simp_names[[k]], tbl, by = "xlsx_sheet", all.y = TRUE)
  #   tbl <- tbl[order(tbl$.i), ]
  #   tbl$.i <- NULL
  return(tbl)
})
names(code_tbl) <- c("diag", "proc")
rm(sheets)

# Check if some xlsx sheet names are used as acronym
code_tbl$diag %>%
  {list(.["xlsx_sheet"], na.omit(c(.["supp_col_1"], .["supp_col_2"])))} %>%
  {any(.[[1]] %in% .[[2]]) | any(.[[2]] %in% .[[1]])} %>%
  {if (.) stop("some xlsx sheet names are used as acronym")}

# Tables of dx and ttt indicators
tbls <- mclapply(1:2, function(k) {
  if (k == 1) {
    code <- "ICD10GM2_code2"
    princ <- "ofs_4_4.2.V010_Princ"
    r <- "(?=^.*?4\\.2(\\.)?V([0-5][0-9]0).*$)(?!^.*Princ$)"
  } else {
    code <- "Code_CHOP2"
    princ <- "ofs_155_4.3.V010_Trait princ"
    r <- "(?=^.*?4\\.3\\.V([0-9]{2}|100)0_.*$)(?!^.*princ$)"
  }
  v <- grep(r, names(ofs), perl = TRUE, value = T)
  codes_ofs <- ofs[, c(princ, v), with = FALSE] %>%
    as.matrix() %>% as.vector() %>% unique()
  code_list_1 <- code_tbl[[k]][[code]] %>%
    lapply(function(z) grep(paste0("^", z), codes_ofs, value = T)) %>%
    unlist() %>%
    c(code_tbl[[k]][[code]]) %>%
    unique() %>%
    sort() %>%
    setNames(., .) %>%
    as.list()
  code_list_2 <- c("xlsx_sheet", paste0("supp_col_", 1:2)) %>%
    {code_tbl[[k]][.]} %>%
    as.matrix() %>%
    as.vector() %>%
    unique() %>%
    sort() %>%
    setNames(., .) %>%
    lapply(function(z) {
      sort(unique(unlist(lapply(0:2, function(j) {
        u <- if (j == 0) "xlsx_sheet" else paste0("supp_col_", j)
        b <- code_tbl[[k]][[u]] %in% z
        code_tbl[[k]][[code]][b]
      }))))
    })
  code_list <- c(code_list_1, code_list_2)
  DX2 <- as.matrix(ofs[, ..v])
  M <- do.call(cbind, lapply(names(code_list), function(x) {
    p <- paste0("^(", paste(code_list[[x]], collapse = "|"), ")")
    d <- data.frame(
      ofs[[princ]] %>% {!is.na(.) & grepl(p, .)},
      apply(DX2 %>% {!is.na(.) & grepl(p, .)}, 1, sum)
    )
    names(d) <- paste0(x, c("_Main", "_Other"))
    return(d)
  }))
  cbind(ofs[, .(p_IPP, sej_NUMERO_SEJOUR)], M)
})
ofs_dx <- tbls[[1]]
ofs_ttt <- tbls[[2]]
rm(tbls)

# Save databases
out_dir <- "data/ofs_dx_ttt_indic_20201112"
if(!file.exists(out_dir)) dir.create(out_dir)
save(ofs_dx, file = file.path(out_dir, "ofs_dx.rda"))
save(ofs_ttt, file = file.path(out_dir, "ofs_ttt.rda"))
rm(out_dir)

# Session Info
si_dir <- "sessionInfo_files"
if(!file.exists(si_dir)) dir.create(si_dir)
sink(file.path(si_dir, "ofs_dx_ttt_indic_sessionInfo_20201112.txt"))
print(sessionInfo(), locale = FALSE)
sink()
rm(si_dir)
