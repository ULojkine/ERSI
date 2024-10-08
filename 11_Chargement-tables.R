variables_a_retirer = c("PROPLOCA", "PY200G", "PY021N", "PY021G", "Z01Q0P",
                        "HANDICH", "HANDICG")

variables_a_selectionner <- c("DIM", "AGE", "CS24", "CS_ANTE", "DIP11","DIP14","SEXE", "SITUA", "PB040")

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

labellize <- function(x) {
  l <- length(x)
  paste(x[-l], x[-1]-1, sep="-")
}

breaks_cat <- c(16, 25, 50, 55+5*(0:6), Inf)
breaks_qqual <- c(16, 20+5*(0:13), Inf)
breaks_qqual_regroupe <- c(16, 30, 40+5*(0:7), Inf)

adultes <- individus %>%
  filter(AGE >= 16) %>% 
  mutate(
    limitations = factor(recode(
      DIM, `1` = "1 - Oui, fortement limité(e)",
      `2` = "2 - Oui, limité(e), mais pas fortement",
      `3` = "3 - Non, pas limité(e) du tout"
    )),
    limite = DIM == 1 | DIM == 2,
    limite_forte = DIM == 1,
    age_cat = cut(AGE, breaks = breaks_cat, labels=labellize(breaks_cat), right=FALSE),
    age_qqual = cut(AGE, breaks = breaks_qqual_regroupe, labels=labellize(breaks_qqual_regroupe), right=FALSE),
    age_qqual_nonregroupe = cut(AGE, breaks = breaks_qqual, labels=labellize(breaks_qqual), right=FALSE),
    CS = substr(CS24, 1, 1),
    CS = ifelse(CS == 7, substr(CS_ANTE, 1, 1), CS),
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
    retraite = SITUA == 5
  )

saveRDS(adultes, "./interm/adultes.rds")