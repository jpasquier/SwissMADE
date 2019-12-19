# Libraries
pkgs <- c("tcltk", "data.table", "parallel", "magrittr", "writexl")
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
Dir <- "data/PNR74_extractions_donnees_structurees_20190910"
if(!file.exists(Dir)) Dir <- tk_choose.dir(caption = "Select data directory")

# Load ofs database
f0 <- "ofs.rda"
f <- file.path(getwd(), Dir, f0)
if(!file.exists(f)) stop(paste("cannot find", f0, "in data directory"))
load(f)
rm(Dir, f0, f)

# ofs - diagnostics / procedure
# profiles
profiles <- mclapply(1:2, function(k) {
  if (k == 1) {
    r <- ".*?4\\.2(\\.)?V([0-5][0-9]0).*"
    V <- grep(r, names(ofs), value = T) %>%
      {.[order(as.numeric(sub(r, "\\2", .)))]}
    W <- paste0("DX", c("P", "C", sprintf("%02d", 1:49)))
  } else {
    r <- ".*?4\\.3\\.V([0-9]{2}|100)0_.*"
    V <- grep(r, names(ofs), value = T) %>%
      {.[order(as.numeric(sub(r, "\\1", .)))]}
    W <- paste0("TTT", c("P", sprintf("%02d", 1:99)))
  }
  tbl <- ofs[, ..V] %>% 
    {!(is.na(.) | . == "")} %>% 
    as.data.table() %>%
    setNames(W) %>%
    {.[, .(freq = .N), by = W]} %>%
    as.data.frame() %>%
    {.[order(.$freq, decreasing = TRUE), c(ncol(.), 1:(ncol(.) - 1))]}
  return(list(tbl, data.frame(name = W, original_name = V)))
})
profiles <- unlist(profiles, recursive = FALSE)
profiles <- setNames(profiles, c("ofs_diag_profiles", "ofs_diag_names",
                                 "ofs_proc_profiles", "ofs_proc_names"))

# Export results and session info
if (!file.exists("results")) dir.create("results")
write_xlsx(profiles, "results/ofs_diag_proc_profiles_20191120.xlsx")
sink("results/sessionInfo_20191120.txt")
print(sessionInfo(), locale = FALSE)
sink()

