library(DBI)
setwd("~/Projects/SwissMADE/")
password_file <- "data-raw/baden/baden_database_password.txt"
password <- readChar(password_file, file.info(password_file)$size - 1)
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "database",
  host = "localhost",
  port = 5432,
  password = password,
  user = "postgres"
)
tbl_list <- dbListTables(con)
for (tbl in tbl_list) {
  assign(tbl, dbReadTable(con, tbl))
  save(list = tbl, file = paste0("data/baden/", tbl, ".rda"), compress = "xz")
  rm(list = tbl)
}
