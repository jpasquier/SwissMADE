# Libraries
pkgs <- c("tcltk", "parallel", "magrittr", "dplyr", "writexl")
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

# Load ofs, ofs_dx and ofs_ttt databases
for (f0 in c("ofs.rda", "ofs_dx.rda",  "ofs_ttt.rda")) {
  f <- file.path(getwd(), data_dir, f0)
  if(!file.exists(f)) stop(paste("cannot find", f0, "in data directory"))
  load(f)
}
rm(data_dir, f0, f)

# Create Event and EI indicators
if (FALSE) names(ofs_dx)[names(ofs_dx) %in% names(ofs_ttt)]
if (FALSE) names(ofs_ttt)[names(ofs_ttt) %in% names(ofs_dx)]
EI <- ofs %>%
  select(p_IPP, sej_NUMERO_SEJOUR, `ofs_27_1.5.V02_Décision sortie`) %>%
  rename(sortie = `ofs_27_1.5.V02_Décision sortie`) %>%
  full_join(ofs_dx, by = c("p_IPP", "sej_NUMERO_SEJOUR")) %>%
  full_join(ofs_ttt, by = c("p_IPP", "sej_NUMERO_SEJOUR")) %>%
  mutate(
    Transfu = Transfu_Main | Transfu_Other > 0,
    ADE_Hemo = ADE_Hemo_Main | ADE_Hemo_Other > 0,
    ADE_VTE = ADE_VTE_Main | ADE_VTE_Other > 0,
    ADE_ATE = ADE_ATE_Main | ADE_ATE_Other > 0,
    Hemo = Hemo_Main | Hemo_Other > 0 | Hemo_Traum_Main | Hemo_Traum_Other > 0,
    Sev_Hemo = (Sev_Hemo_Main | Sev_Hemo_Other > 0) |
      Hemo & (sortie == 5 | Transfu),
    DVT = DVT_Main | DVT_Other > 0,
    PE = PE_Main | PE_Other > 0,
    AMI = AMI_Main | AMI_Other > 0,
    Stroke = Stroke_Main | Stroke_Other > 0,
    EI_Hemo = Hemo & ADE_Hemo,
    EI_Sev_Hemo = Sev_Hemo & ADE_Hemo,
    EI_DVT = DVT & ADE_VTE,
    EI_PE = PE & ADE_VTE,
    EI_AMI = AMI & ADE_ATE,
    EI_Stroke = Stroke & ADE_ATE,
    Event_pos =  Hemo | Sev_Hemo | DVT | PE |  AMI | Stroke,
    Event_neg = !Event_pos,
    EI_pos = EI_Hemo | EI_Sev_Hemo | EI_DVT | EI_PE | EI_AMI | EI_Stroke,
    EI_neg = !EI_pos
  ) %>%
  select(p_IPP, sej_NUMERO_SEJOUR, Hemo, Sev_Hemo, DVT, PE, AMI, Stroke,
         starts_with("EI_"), starts_with("Event_"))

# Samples stays with and without adverse events
set.seed(666)
smpl <- data.frame(sample = as.character(c()),
                   sej_NUMERO_SEJOUR = as.numeric(c()))
X <- c("Hemo", "Sev_Hemo", "DVT", "PE", "AMI", "Stroke")
smpl <- do.call(rbind, lapply(X, function(x) {
  id_event_pos <- EI[EI[[x]], "sej_NUMERO_SEJOUR"]
  event_pos <- data.frame(
    sample = paste0(x, "_pos"),
    sej_NUMERO_SEJOUR = sample(id_event_pos, min(36, length(id_event_pos)))
  )
  id_event_neg <- EI[!EI[[x]], "sej_NUMERO_SEJOUR"]
  event_neg <- data.frame(
    sample = paste0(x, "_neg"),
    sej_NUMERO_SEJOUR = sample(id_event_neg, 100 - nrow(event_pos))
  )
  z <- paste0("EI_", x)
  id_ei_pos <- EI[EI[[z]], "sej_NUMERO_SEJOUR"]
  ei_pos <- data.frame(
    sample = paste0(z, "_pos"),
    sej_NUMERO_SEJOUR = sample(id_ei_pos, min(36, length(id_ei_pos)))
  )
  id_ei_neg <- EI[!EI[[z]], "sej_NUMERO_SEJOUR"]
  ei_neg <- data.frame(
    sample = paste0(z, "_neg"),
    sej_NUMERO_SEJOUR = sample(id_ei_neg, 100 - nrow(ei_pos))
  )
  rbind(event_pos, event_neg, ei_pos, ei_neg)
}))
smpl <- rbind(
  smpl, 
  data.frame(
   sample = "All_Event_neg",
   sej_NUMERO_SEJOUR = sample(EI[EI$Event_neg, "sej_NUMERO_SEJOUR"], 64)
  ),
  data.frame(
   sample = "All_EI_neg",
   sej_NUMERO_SEJOUR = sample(EI[EI$EI_neg, "sej_NUMERO_SEJOUR"], 64)
  )
)
table(smpl$sample)

# Results
write_xlsx(smpl, "results/sampling_stays_20200708.xlsx")
sink("sessionInfo_files/sampling_stays_sessionInfo_20200708.txt")
print(sessionInfo(), locale = FALSE)
sink()

#
table(aggregate(sample ~ sej_NUMERO_SEJOUR, smpl, length)[, 2])


