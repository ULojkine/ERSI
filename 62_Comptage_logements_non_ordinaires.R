library(tidyverse)
library(arrow)
library(duckdb)
library(openxlsx)

con <- dbConnect(duckdb())
rp2019 <- tbl(con, "read_csv('data/FD_INDREG_2019.csv', types={'CATL': 'VARCHAR'})")

calculer_repartition_pcs <- function(matable_duckdb) {
  matable_duckdb %>% 
    group_by(CS1, AGED) %>% 
    mutate(ordinaire = as.integer(CATL == "1")) %>% 
    summarise(
      nobs_echant = n(),
      ordinaires = sum(ordinaire*IPONDI),
      non_ordinaires = sum((1-ordinaire)*IPONDI)
    ) %>% 
    mutate(prop = non_ordinaires/(ordinaires+non_ordinaires)) %>% 
    collect() %>% 
    arrange(CS1, AGED)
}

calculer_repartition_dip <- function(matable_duckdb) {
  matable_duckdb %>% 
    mutate(
      ordinaire = as.integer(CATL == "1"),
      diplome_regroupe = case_match(
        DIPL,
        c("ZZ", "01", "02", "03", "11") ~ "01 - Sans",
        "12" ~ "02 - Brevet",
        "13" ~ "03 - CAP",
        c("14", "15") ~ "04 - Bac",
        c("16", "17", "18", "19") ~ "05 - Supérieur")
    ) %>% 
    group_by(diplome_regroupe, AGED) %>% 
    summarise(
      nobs_echant = n(),
      ordinaires = sum(ordinaire*IPONDI),
      non_ordinaires = sum((1-ordinaire)*IPONDI)
    ) %>% 
    mutate(prop = non_ordinaires/(ordinaires+non_ordinaires)) %>% 
    collect() %>% 
    arrange(diplome_regroupe, AGED)
}

repartition2019 <- list(
  CS1 = calculer_repartition_pcs(rp2019),
  DIPL = calculer_repartition_dip(rp2019)
)

write.xlsx(
  repartition2019, 
  "output/comptage_CS1-DIPL_AGED_ordinaires_2019.xlsx"
  )
