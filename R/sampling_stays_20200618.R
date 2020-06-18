# Libraries
pkgs <- c("tcltk", "readxl", "writexl")
npkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(npkgs)) install.packages(npkgs)
for (pkg in pkgs) library(pkg, character.only = TRUE)
rm(pkg, pkgs, npkgs)

# Working directory
wd <- "~/Projects/SwissMADE"
if(!file.exists(wd)) wd <- tk_choose.dir(caption = "Select working directory")
setwd(wd)
rm(wd)

# Loads results of ofs_ei_att_indic_20200428.R
EI <- as.data.frame(read_xlsx("results/ofs_ei_att_indic_20200428.xlsx"))

# Samples stays with and without adverse events
set.seed(666)
smpl <- data.frame(sample = as.character(c()),
                   sej_NUMERO_SEJOUR = as.numeric(c()))
X <- paste0("EI_", c("PE", "Stroke", "AMI", "Sev_Hemo"))
EI$NEG2 <- apply(!EI[X], 1, all)   # sum(EI$NEG2) -> 7782
for (x in paste0("EI_", c("PE", "Stroke", "AMI", "Sev_Hemo"))) {
  s <- "sej_NUMERO_SEJOUR"
  n <- EI[EI[[x]] & !(EI[[s]] %in% smpl[[s]]), s]
  pos <- data.frame(
    sample = paste0(sub("^EI_", "", x), "_pos"),
    sej_NUMERO_SEJOUR = sample(n, min(36, length(n)))
  )
  n <- EI[EI$NEG2 & !(EI[[s]] %in% smpl[[s]]), s]
  neg <- data.frame(
    sample = paste0(sub("^EI_", "", x), "_neg"),
    sej_NUMERO_SEJOUR = sample(n, 100 - nrow(pos))
  )
  smpl <- rbind(smpl, pos, neg)
}
table(smpl$sample)

# Results
write_xlsx(smpl, "results/sampling_stays_20200618.xlsx")
sink("sessionInfo_files/sessionInfo_20200618.txt")
print(sessionInfo(), locale = FALSE)
sink()


