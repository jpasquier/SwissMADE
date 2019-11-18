library(data.table)

setDTthreads(0)

files <- grep("_idvar\\.rds$", list.files(), value = T)
id_vars <- do.call(rbind, lapply(files, function(f) {
 cbind(db = sub("_idvar\\.rds$", "", f), readRDS(f)[, n_dup := NULL])
}))
write.table(id_vars, "id_var_20191016.csv", sep = ",")

