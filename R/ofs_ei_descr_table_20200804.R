# Libraries
pkgs <- c("tcltk", "rlang", "dplyr", "writexl")
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
  mutate(
    sortie = `ofs_27_1.5.V02_Décision sortie`,
    All_stays = TRUE,
    date_admission = as.POSIXct(`ofs_16_1.2.V01_Date admission`),
    date_sortie = as.POSIXct(as.character(`ofs_26_1.5.V01_Date sortie`),
                             format = "%Y%m%d%H"),
    duree_sej = (date_sortie - date_admission) / 24
  ) %>%
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
  )

# Descriptive table
X <- c("All_stays", "Transfu", "ADE_Hemo", "ADE_VTE", "ADE_ATE", "Hemo",
       "Sev_Hemo", "DVT", "PE", "AMI", "EI_Hemo", "EI_Sev_Hemo", "EI_DVT",
       "EI_PE", "EI_AMI", "EI_Stroke", "Event_pos", "Event_neg", "EI_pos",
       "EI_neg")
tab <- sapply(X, function(x) {
  EI %>%
    mutate(All_stays = TRUE) %>%
    filter(!! sym(x)) %>%
    summarise(
      nb_sej = n(),
      nb_pat = length(unique(p_IPP)),
      sex_male_n = sum(ofs_11_1.1.V01_Sexe == 1),
      sex_male_p = mean(ofs_11_1.1.V01_Sexe == 1),
      age_adm_mean = mean(ofs_13_1.1.V03_Age),
      age_adm_sd = sd(ofs_13_1.1.V03_Age),
      age_adm_median = median(ofs_13_1.1.V03_Age),
      age_adm_iqr = IQR(ofs_13_1.1.V03_Age),
      age_adm_min = min(ofs_13_1.1.V03_Age),
      age_adm_max = max(ofs_13_1.1.V03_Age),
      adm_mode_emergency_n = sum(`ofs_18_1.2.V03_Mode admission` == 1),
      adm_mode_emergency_p = mean(`ofs_18_1.2.V03_Mode admission` == 1),
      adm_mode_planned_n = sum(`ofs_18_1.2.V03_Mode admission` == 2),
      adm_mode_planned_p = mean(`ofs_18_1.2.V03_Mode admission` == 2),
      adm_mode_intern_transfert_n = sum(`ofs_18_1.2.V03_Mode admission` == 4),
      adm_mode_intern_transfert_p = mean(`ofs_18_1.2.V03_Mode admission` == 4),
      adm_mode_transfert_24h_n = sum(`ofs_18_1.2.V03_Mode admission` == 5),
      adm_mode_transfert_24h_p = mean(`ofs_18_1.2.V03_Mode admission` == 5),
      after_exit_deceased_n = sum(`ofs_29_1.5.V04_Prise en ch sortie` == 0),
      after_exit_deceased_p = mean(`ofs_29_1.5.V04_Prise en ch sortie` == 0),
      after_exit_cured_n = sum(`ofs_29_1.5.V04_Prise en ch sortie` == 1),
      after_exit_cured_p = mean(`ofs_29_1.5.V04_Prise en ch sortie` == 1),
      after_exit_ambu_care_n = sum(`ofs_29_1.5.V04_Prise en ch sortie` == 2),
      after_exit_ambu_care_p = mean(`ofs_29_1.5.V04_Prise en ch sortie` == 2),
      after_exit_home_care_n = sum(`ofs_29_1.5.V04_Prise en ch sortie` == 3),
      after_exit_home_care_p = mean(`ofs_29_1.5.V04_Prise en ch sortie` == 3),
      after_exit_statio_care_n = sum(`ofs_29_1.5.V04_Prise en ch sortie` == 4),
      after_exit_statio_care_p =
        mean(`ofs_29_1.5.V04_Prise en ch sortie` == 4),
      after_exit_rehab_n = sum(`ofs_29_1.5.V04_Prise en ch sortie` == 5),
      after_exit_rehab_p = mean(`ofs_29_1.5.V04_Prise en ch sortie` == 5),
      after_exit_other_n = sum(`ofs_29_1.5.V04_Prise en ch sortie` == 8),
      after_exit_other_p = mean(`ofs_29_1.5.V04_Prise en ch sortie` == 8),
      after_exit_unknown_n = sum(`ofs_29_1.5.V04_Prise en ch sortie` == 9),
      after_exit_unknown_p = mean(`ofs_29_1.5.V04_Prise en ch sortie` == 9),
      los_days_mean = mean(duree_sej),
      los_days_sd = sd(duree_sej),
      los_days_median = median(duree_sej),
      los_days_iqr = IQR(duree_sej),
      los_days_min = min(duree_sej),
      los_days_max = max(duree_sej),
    ) %>%
    unlist()
}) %>%
  as.data.frame() %>%
  {cbind(X = rownames(.), .)}
rm(X)

# Export results
write_xlsx(tab, "results/ofs_ei_descr_table_20200804.xlsx")
if(FALSE) {
sink("sessionInfo_files/.....txt")
print(sessionInfo(), locale = FALSE)
sink()
}

