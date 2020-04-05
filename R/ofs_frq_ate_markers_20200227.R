# Libraries
pkgs <- c("tcltk", "parallel", "magrittr", "writexl")
npkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(npkgs)) install.packages(npkgs)
for (pkg in pkgs) library(pkg, character.only = TRUE)
rm(pkg, pkgs, npkgs)

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

# Load ofs_dx and ofs_ttt databases
for (f0 in c( "ofs_dx.rda",  "ofs_ttt.rda")) {
  f <- file.path(getwd(), data_dir, f0)
  if(!file.exists(f)) stop(paste("cannot find", f0, "in data directory"))
  load(f)
}
rm(data_dir, f0, f)

# Compute code frequencies
frq_tbl <- lapply(list(dx = ofs_dx, ttt = ofs_ttt), function(db) {
  ## Consider the databases as data.frame (instead data.table)
  db <- as.data.frame(db)
  ## List of codes
  X <- sub("_Main$", "", grep("_Main$", names(db), value = TRUE))
  ## Cast "other" indicators from numeric to logical
  for (x in X) {
    db[[paste0(x, "_Other")]] <- as.logical(db[[paste0(x, "_Other")]])
  }
  ## Add "any" indicators
  A <- sapply(X, function(x) {
    apply(db[paste0(x, c("_Main", "_Other"))], 1, any)
  })
  colnames(A) <- paste0(colnames(A), "_Any")
  db <- cbind(db, A)
  ## Frequencies
  frq <- db %>%
    {sapply(.[, !(names(.) %in% c("p_IPP", "sej_NUMERO_SEJOUR"))], sum)} %>%
    {data.frame(Code = names(.), Count = ., Prop = . / nrow(db))} %>%
    {.[order(.$Count, decreasing = TRUE), ]}
  rownames(frq) <- NULL
  return(frq)
})

# Export frequencies
write_xlsx(frq_tbl, "results/ofs_frq_ate_markers_20200227.xlsx")
