library(parallel)
library(data.table)
library(dplyr)
library(writexl)

options(mc.cores = detectCores())

# Working directory
setwd("~/Projects/SwissMADE")

# Data
DB <- c("chop", "cim10", "molis_smoker", "molis", "mouvement",
        "ofs", "predimed", "soarian_frcv", "soarian_med",
        "soarian_sg_occ", "soarian")
for(db in DB) load(paste0("data/", db, ".rda"))
rm(db)

# Codes associated to geriatric stays
unique(mouvement[, c("mvt_CODE_UF_RESP_MEDICALE", "mvt_LIB_UF_RESP_MEDICALE")])
grep("(G|g)éria", unique(mouvement$mvt_LIB_UF_RESP_MEDICALE), value = TRUE)

# Number of movements for stays with at least one geriatric movement
nb_mvt_ger <- mouvement %>%
  as.data.frame() %>%
  mutate(GER = mvt_CODE_UF_RESP_MEDICALE %in% c("GERH", "SYPH")) %>%
  filter(sej_NUMERO_SEJOUR %in% unique(.[GER, "sej_NUMERO_SEJOUR"])) %>%
  {
    full_join(
      aggregate(GER ~ sej_NUMERO_SEJOUR, ., sum),
      aggregate(!GER ~ sej_NUMERO_SEJOUR, ., sum),
      by = "sej_NUMERO_SEJOUR"
    )
  } %>%
  rename(mvt_ger = GER, mvt_autre = `!GER`)

# Select in each table the stays with at least one geriatric movement
tbls <- mclapply(setNames(DB, DB), function(db) {
  get(db)[sej_NUMERO_SEJOUR %in% nb_mvt_ger$sej_NUMERO_SEJOUR, ]
})

# Add nb_mvt_ger
tbls$nb_mvt_ger <- nb_mvt_ger

# Export tables
f <- "extraction_geriatrie.xlsx"
d <- "data/extraction_geriatrie_20201130"
if (!dir.exists(d)) dir.create(d)
write_xlsx(tbls, file.path(d, f))

# Read me
readme <- paste(
"L'unité d'hospitalisation est définie pour un mouvement. Un séjour peut être
composé de plusieurs mouvements. Les observations de chaque table des données
SwissMADE sont associés aux séjours (et non pas aux mouvement). La présente
extraction est constituée de toutes les observations associés aux séjours
contenant au moins un mouvement en gériatrie. Les mouvements pour lesquels
l'unité d'hospitalisation est la gériatrie sont ceux pour lesquels la
variable 'mvt_CODE_UF_RESP_MEDICALE' de la table 'mouvement' est égale à 'GERH'
ou 'SYPH'. Il s'agit de", nrow(nb_mvt_ger), "séjours comprenant",
sum(nb_mvt_ger[c("mvt_ger", "mvt_autre")]), "mouvements dont",
sum(nb_mvt_ger$mvt_ger), "en gériatrie. La table supplémentaire 'nb_mvt_ger'
indique pour chaque séjour le nombre de mouvements en gériatrie et dans les
autres services."
)
f <- sub("xlsx$", "txt", f)
sink(file.path("/tmp", f))
cat(readme)
sink()
system(paste("fold -w 80 -s", file.path("/tmp", f), ">", file.path(d, f)))
f <- file.remove(file.path("/tmp", f))
rm(d, f)
