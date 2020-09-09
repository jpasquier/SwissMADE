library(readxl)
library(writexl)

setwd("~/Projects/SwissMADE")

load("data/ofs.rda")
load("data/ofs_dx.rda")
load("data/ofs_ttt.rda")
smp <- as.data.frame(read_xlsx("results/sampling_stays_20200708.xlsx"))

v_dx <- names(ofs_dx)[!(names(ofs_dx) %in% c("p_IPP", "sej_NUMERO_SEJOUR"))]
smp <- cbind(smp, dx = sapply(smp$sej_NUMERO_SEJOUR, function(nsej) {
  a <- ofs_dx[ofs_dx$sej_NUMERO_SEJOUR == nsej, v_dx]
  a <- names(a)[a > 0]
  a <- unique(gsub("_(Main|Other)$", "", a))
  paste(a, collapse = "; ")
}))

v_ttt <- names(ofs_ttt)[!(names(ofs_ttt) %in% c("p_IPP", "sej_NUMERO_SEJOUR"))]
smp <- cbind(smp, ttt = sapply(smp$sej_NUMERO_SEJOUR, function(nsej) {
  a <- ofs_ttt[ofs_ttt$sej_NUMERO_SEJOUR == nsej, v_ttt]
  a <- names(a)[a > 0]
  a <- unique(gsub("_(Main|Other)$", "", a))
  paste(a, collapse = "; ")
}))

dx_princ <- "ofs_4_4.2.V010_Princ"
r <- "(?=^.*?4\\.2(\\.)?V([0-5][0-9]0).*$)(?!^.*Princ$)"
dx_other <- grep(r, names(ofs), perl = TRUE, value = T)
ttt_princ <- "ofs_155_4.3.V010_Trait princ"
r <- "(?=^.*?4\\.3\\.V([0-9]{2}|100)0_.*$)(?!^.*princ$)"
ttt_other <- grep(r, names(ofs), perl = TRUE, value = T)
v_ofs <- c("sej_NUMERO_SEJOUR", dx_princ, dx_other, ttt_princ, ttt_other)
ofs_data <- subset(ofs, sej_NUMERO_SEJOUR %in% smp$sej_NUMERO_SEJOUR, v_ofs)

write_xlsx(list(codes = smp, ofs_data = ofs_data),
           "results/sampling_stays_extract_codes_20200907.xlsx")
