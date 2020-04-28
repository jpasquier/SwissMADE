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

# Create EI indicators
#
# Courriel MA Le Pogam 28.02.2020
#
# * Pour identifier les hémorragies (all) associées aux ATT :croiser listes
#   Hemo et ADE_Hemo (au moins un code de chaque liste codé dans le séjour)
#
# * Pour identifier les hémorragies traumatiques associées aux ATT : croiser
#   listes Hemo_Traum et ADE_Hemo (au moins un code diagnostique de chaque
#   liste codé dans le séjour)
#
# * Pour identifier les hémorragies sévères traumatiques ou non associées aux
#   ATT :
#   ** identifier d’abord les séjours avec hémorragie sévère : au moins un
#      code de la liste Sev_Hemo ou au moins un code de la liste Hemo et (mode
#      de sortie décès (variable 1.5.V02 Décision de sortie = 5) ou au moins
#      un code de traitement dans la liste Transfu)
#   ** puis croiser avec liste ADE_Hemo (au moins un diagnostic diagnostique
#      des séjours avec hémorragie sévère dans la liste ADE_Hemo)
#
# * Pour identifier les thromboses veineuses profondes des membres inférieurs
#   associées aux ATT : croiser listes DVT et ADE_VTE (au moins un code
#   diagnostique de chaque liste codé dans le séjour)
#
# * Pour identifier les embolies pulmonaires associées aux ATT : croiser
#   listes PE et ADE_VTE (au moins un code  diagnostique de chaque liste codé
#   dans le séjour)
#
# * Pour identifier les évènements thromboemboliques artériels associés aux
#   ATT : croiser les listes ATE et ADE_ATE (au moins un code  diagnostique de
#   chaque liste codé dans le séjour)
#
# * Idem pour les sous-catégories suivantes d’évènements thromboemboliques
#   artériels : infarctus aigu du myocarde (AMI) et accident vasculaire
#   cérébral ischémique (stroke).
if (FALSE) names(ofs_dx)[names(ofs_dx) %in% names(ofs_ttt)]
if (FALSE) names(ofs_ttt)[names(ofs_ttt) %in% names(ofs_dx)]
EI <- ofs %>%
  select(p_IPP, sej_NUMERO_SEJOUR, `ofs_27_1.5.V02_Décision sortie`) %>%
  rename(sortie = `ofs_27_1.5.V02_Décision sortie`) %>%
  full_join(ofs_dx, by = c("p_IPP", "sej_NUMERO_SEJOUR")) %>%
  full_join(ofs_ttt, by = c("p_IPP", "sej_NUMERO_SEJOUR")) %>%
  mutate(
    ADE_ATE = ADE_ATE_Main | ADE_ATE_Other > 0,
    ADE_Hemo = ADE_Hemo_Main | ADE_Hemo_Other > 0,
    ADE_VTE = ADE_VTE_Main | ADE_VTE_Other > 0,
    AMI = AMI_Main | AMI_Other > 0,
    ATE = ATE_Main | ATE_Other > 0,
    DVT = DVT_Main | DVT_Other > 0,
    Hemo = Hemo_Main | Hemo_Other > 0,
    Hemo_Traum = Hemo_Traum_Main | Hemo_Traum_Other > 0,
    PE = PE_Main | PE_Other > 0,
    Sev_Hemo = Sev_Hemo_Main | Sev_Hemo_Other > 0,
    Stroke = Stroke_Main | Stroke_Other > 0,
    Transfu = Transfu_Main | Transfu_Other > 0,
    EI_Hemo = Hemo & ADE_Hemo,
    EI_Hemo_Traum = Hemo_Traum & ADE_Hemo,
    EI_Sev_Hemo = (Sev_Hemo | (Hemo & (sortie == 5 | Transfu))) & ADE_Hemo,
    EI_DVT = DVT & ADE_VTE,
    EI_PE = PE & ADE_VTE,
    EI_ATE = ATE & ADE_ATE,
    EI_AMI = AMI & ADE_ATE,
    EI_Stroke = Stroke & ADE_ATE,
    EI_pos = EI_Hemo | EI_Hemo_Traum | EI_Sev_Hemo | EI_DVT | EI_PE | EI_ATE |
      EI_AMI | EI_Stroke,
    EI_neg = !EI_pos,
    Event_pos = ADE_ATE | ADE_Hemo | ADE_VTE | AMI | ATE | DVT | Hemo |
      Hemo_Traum | PE | Sev_Hemo | Stroke | Transfu,
    Event_neg = !Event_pos
  ) %>%
  select(p_IPP, sej_NUMERO_SEJOUR, starts_with("EI_"), starts_with("Event_"))

# Summarise
summarise_at(EI, mean, .vars = vars(starts_with("EI_"), starts_with("Event_")))
summarise_at(EI, sum, .vars = vars(starts_with("EI_"), starts_with("Event_")))

# Export indicators
write_xlsx(EI, "results/ofs_ei_att_indic_2020.xlsx")
