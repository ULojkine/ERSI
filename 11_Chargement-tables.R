# Fonctions
labellize <- function(x) {
  l <- length(x)
  paste(x[-l], x[-1]-1, sep="-")
}

# breaks_cat <- c(16, 25, 50, 55+5*(0:6), Inf)
# breaks_qqual_regroupe <- c(16, 30, 40+5*(0:7), Inf)

mise_en_forme <- function(individus){
  individus %>%
    filter(AGE >= 16) %>% 
    mutate(
      limitations = factor(recode(
        DIM, `1` = "1 - Oui, fortement limité(e)",
        `2` = "2 - Oui, limité(e), mais pas fortement",
        `3` = "3 - Non, pas limité(e) du tout",
        .default=NA_character_
      )),
      limite = DIM == 1 | DIM == 2,
      limite_forte = DIM == 1,
      CS = substr(CS24, 1, 1),
      CS = ifelse(CS == 7 | is.na(CS), substr(CS_ANTE, 1, 1), CS),
      PCS = factor(recode(
        CS,
        `1` = "Agriculteurs",
        `2` = "Artisans",
        `3` = "Cadres",
        `4` = "Prof inter",
        `5` = "Employés",
        `6` = "Ouvriers",
        `7` = "Retraités",
        `8` = "Inactifs",
        .default = NA_character_
      )),
      diplome = ifelse(is.na(DIP11), DIP14, DIP11), # jusqu'à 2013, le diplôme est encodé en 14 catégories. le recodage qui suit fonctionne pour les deux encodages
      diplome = factor(case_when(
        diplome <= 33 ~ "Supérieur",
        diplome %in% c(41:43) ~ "Bac",
        diplome == 50 ~ "CAP",
        diplome %in% c(60:70) ~ "Brevet",
        diplome == 71 ~ "Sans",
        .default = NA
      )),
      Sexe = fct_recode(factor(SEXE), Hommes = "1", Femmes = "2"),
      retraite_brut = SITUA == 5,
      retraite = retraite_brut | AGE >= 70, # dans l'exercice principal, on considère tout le monde retraité à partir de 70 ans
      retraite_revenu_i_brut = !is.na(RETRAITES_I),
      retraite_revenu_i = retraite_revenu_i_brut | AGE >= 70
    ) %>%
    select(c("AGE","Sexe","PB040","AENQ","limite","limite_forte","PCS","diplome","SITUA","retraite","retraite_brut","retraite_revenu_i"))
}

# SRCV 
variables_a_retirer = c("PROPLOCA", "PY200G", "PY021N", "PY021G", "Z01Q0P",
                        "HANDICH", "HANDICG")

variables_a_selectionner <- c("DIM", "AGE", "CS24", "CS_ANTE", "DIP11","DIP14","SEXE", "SITUA", "RETRAITES_I","RRET_a", "PB040")

individus <- list(
  `2008` = "individus08_diff",
  `2009` = "individus09_diff",
  `2010` = "individus10_diff",
  `2011` = "individus11_diff",
  `2012` = "INDIVIDUS12_DIFFV2",
  `2013` = "INDIVIDUS13_DIFFV2",
  `2014` = "INDIVIDUS14_DIFF",
  `2015` = "INDIVIDUS15_DIFF",
  `2016` = "individus16_diff",
  `2017` = "INDIVIDUS17_DIFF",
  `2018` = "INDIVIDUS18_DIFF",
  `2019` = "INDIVIDUS19_DIFF"
) %>% 
  map(~read_delim(
    paste0("data/SRCV/", ., ".csv"), 
    delim = ";",
    col_select = any_of(c(variables_a_selectionner, tolower(variables_a_selectionner)))
  )) %>% 
  map(rename_with, toupper) %>% 
  map(mutate, across(c(ends_with(c("_F", "_FLAG")),"CS24"), as.character)) %>% 
  map(select, -any_of(variables_a_retirer)) %>% 
  bind_rows(.id="AENQ")

adultes <- mise_en_forme(individus)
saveRDS(adultes, "./interm/adultes_SRCV.rds")
rm(individus)
rm(adultes)
gc()

# Enquête Emploi
variables_a_selectionner <- c("LIMACT","AG", "CSE", "CSA", "DIP11", "SEXE", "SP00", "EXTRIDF")

individus <- NULL
for(a in c(2013:2019)){
  individus_a <- NULL
  for(i in c(1:4)){
    print(paste(a,i))
    individus_a_i <- read_delim(paste0("data/EnqEmploi/indiv",(a-2000),i,".csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
      select(any_of(variables_a_selectionner))
    
    gc()
    
    individus_a <- individus_a %>%
      rbind(individus_a_i)
  }
  individus_a <- individus_a %>%
    mutate(AENQ = a) %>%
    filter(EXTRIDF > 0) # le poids EXTRDIF, qui ne concerne que certaines vagues où la question sur la limitation est posée, est souvent NA ou nul
  individus <- individus %>% rbind(individus_a)
}
rm(individus_a)
rm(individus_a_i)
gc()

individus <- individus %>%
  rename(
    DIM = LIMACT,
    AGE = AG,
    CS24 = CSE,
    CS_ANTE = CSA,
    SITUA = SP00,
    PB040 = EXTRIDF
  ) %>%
  mutate(
    DIP14 = NA,
    RETRAITES_I = NA,
    RRET_a = NA
  )

adultes <- mise_en_forme(individus)
saveRDS(adultes, "./interm/adultes_EnqEmploi.rds")