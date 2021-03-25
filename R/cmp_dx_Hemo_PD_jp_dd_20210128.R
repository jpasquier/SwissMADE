load("~/Projects/SwissMADE/data/ofs.rda")
load("~/Projects/SwissMADE/data/ofs_dx.rda")
load("/tmp/ofs_2.rda")
cmp <- merge(ofs_dx[c("sej_NUMERO_SEJOUR", "Hemo_Main")],
             ofs_2[c("sej_NUMERO_SEJOUR", "Hemo_PD")],
             by = "sej_NUMERO_SEJOUR", all = TRUE)
nrow(cmp)
any(is.na(cmp$Hemo_Main)) | any(is.na(cmp$Hemo_PD))
diff <- merge(cmp[cmp$Hemo_Main != cmp$Hemo_PD, ],
              ofs[c("sej_NUMERO_SEJOUR", "ofs_4_4.2.V010_Princ")],
              by = "sej_NUMERO_SEJOUR")
diff[order(diff$ofs_4_4.2.V010_Princ), ]
