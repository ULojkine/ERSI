adultes <- readRDS("./interm/adultes.rds")

# Calcul de la prévalence moyenne par âge au sein d'une catégorie ... par moyenne glissante
prevalences_moyennes <- function(donnees, age, ...) {
  donnees %>% 
    filter(AGE >= age-2 & AGE <= age+2) %>% 
    group_by(...) %>% 
    summarise(
      AGE = age,
      non_retraite_non_limite = weighted_mean_na_rm(!retraite & !limite, PB040),
      retraite_non_limite = weighted_mean_na_rm(retraite & !limite, PB040),
      non_retraite_limite = weighted_mean_na_rm(!retraite & limite, PB040),
      retraite_limite = weighted_mean_na_rm(retraite & limite, PB040),
      non_retraite_non_limite_forte = weighted_mean_na_rm(!retraite & !limite_forte, PB040),
      retraite_non_limite_forte = weighted_mean_na_rm(retraite & !limite_forte, PB040),
      non_retraite_limite_forte = weighted_mean_na_rm(!retraite & limite_forte, PB040),
      retraite_limite_forte = weighted_mean_na_rm(retraite & limite_forte, PB040),
      .groups='drop'
    ) %>%
    ungroup()
}

# Modèle d'interpolation linéaire des prévalences pour les âges avancés
modele_lineaire <- function(donnees, type_limite) {
  # Appliquer le filtre et ajuster le modèle
  donnees %>% 
    filter(AGE >= 75) %>% 
    lm(reformulate("AGE", response = type_limite), data = ., weights=PB040)
}

# Application du modèle
prevalences_lineaires <- function(donnees){
  data.frame(AGE = c(75:100)) %>%
    mutate(
      retraite_limite = pmin(1, predict(modele_lineaire(donnees, "limite"), newdata = .)),  # Prédire les prévalences, en imposant un max de 1
      retraite_limite_forte = pmin(1, predict(modele_lineaire(donnees, "limite_forte"), newdata = .)),
      retraite_non_limite = 1 - retraite_limite,
      retraite_non_limite_forte = 1 - retraite_limite_forte,
      non_retraite_non_limite = 0, # On suppose que tout le monde est à la retraite aux âges avancés
      non_retraite_limite = 0,
      non_retraite_non_limite_forte = 0,
      non_retraite_limite_forte = 0
    )
}

# SÉRIE LONGUE PAR SEXE & ANNÉE, LISSAGE LINÉAIRE
# On calcule les prévalences pour les âges en dessous de 75 ans par moyenne glissante sur 5 ans
prevalences_SL <- map_dfr(c(30:74), ~prevalences_moyennes(donnees = adultes, age = ., Sexe, AENQ) )

#Au-dessus de 75 ans, on procède par interpolation linéaire dans la catégorie
prevalences_SL_vieux <- adultes %>%
  group_by(Sexe, AENQ) %>%
  do({
    prevalences_lineaires(.)
  }) %>%
  ungroup()

prevalences_SL <- rbind(prevalences_SL, prevalences_SL_vieux) %>%
  arrange(AENQ, Sexe, AGE)

# TABLE PAR PCS, PERIODE, LISSAGE LINÉAIRE
adultes_periodes <- adultes %>%
  filter(AENQ %in% c(2009:2013, 2017:2019)) %>% # on garde les 2 périodes pour lesquelles on a des données de mortalité par PCS et par diplôme
  mutate(
    periode = case_when(
      AENQ %in% c(2009:2013) ~ "2009-2013",
      AENQ %in% c(2017:2019) ~ "2017-2019"
    )
  ) %>%
  group_by(AENQ) %>%
  mutate(
    PB040 = PB040/sum(PB040, na.rm=TRUE)*1000 #On renormalise les poids dans chaque année, de sorte que le total des observations de chaque année compte autant qu'une autre (attention, cela signifie qu'en moyenne, les observations d'une année avec plus d'obs. comptent un peu moins) 
  )%>%
  ungroup()

prevalences_PCS <- map_dfr(c(30:74), ~prevalences_moyennes(donnees = filter(adultes_periodes, PCS %notin% c("Agriculteurs","Artisans",NA,"Inactifs")),
                                                           age = ., Sexe, PCS, periode) )

prevalences_PCS_ensemble <- map_dfr(c(30:74), ~prevalences_moyennes(donnees = adultes_periodes,
                                                                    age = ., Sexe, periode) ) %>%
  mutate(PCS = "Ensemble")

# Les agriculteurs et les artisans ne sont pas assez nombreux, on regroupe les deux sexes
prevalences_PCS_agriculteurs_artisans <- map_dfr(c(30:74), ~prevalences_moyennes(donnees = filter(adultes_periodes, PCS %in% c("Agriculteurs","Artisans")),
                                                                                 age = ., PCS, periode) ) %>%
  mutate(Sexe = "Ensemble")

prevalences_PCS_vieux <- adultes_periodes %>%
  filter(PCS %notin% c("Agriculteurs","Artisans",NA,"Inactifs")) %>%
  group_by(Sexe, PCS, periode) %>%
  do({
    prevalences_lineaires(.)
  }) %>%
  ungroup()

prevalences_PCS_vieux_ensemble <- adultes_periodes %>%
  group_by(Sexe, periode) %>%
  do({
    prevalences_lineaires(.)
  }) %>%
  ungroup() %>%
  mutate(PCS = "Ensemble")

prevalences_PCS_vieux_agriculteurs_artisans <- adultes_periodes %>%
  filter(PCS %in% c("Agriculteurs","Artisans")) %>%
  group_by(PCS, periode) %>%
  do({
    prevalences_lineaires(.)
  }) %>%
  ungroup() %>%
  mutate(Sexe = "Ensemble")


prevalences_PCS <- rbind(prevalences_PCS, prevalences_PCS_ensemble, prevalences_PCS_agriculteurs_artisans, prevalences_PCS_vieux, prevalences_PCS_vieux_ensemble, prevalences_PCS_vieux_agriculteurs_artisans) %>%
  arrange(periode, Sexe, PCS, AGE)

# ggplot(prevalences_PCS, aes(x = AGE, y = retraite_limite + non_retraite_limite,color=periode))+geom_line()+facet_grid(PCS~Sexe)

# TABLE PAR DIPLÔME, PERIODE, LISSAGE LINÉAIRE
prevalences_diplome <- map_dfr(c(30:74), ~prevalences_moyennes(donnees = adultes_periodes, age = ., Sexe, diplome, periode) )

prevalences_diplome_ensemble <- map_dfr(c(30:74), ~prevalences_moyennes(donnees = adultes_periodes,
                                                                    age = ., Sexe, periode) ) %>%
  mutate(diplome = "Ensemble")


prevalences_diplome_vieux <- adultes_periodes %>%
  group_by(Sexe, diplome, periode) %>%
  do({
    prevalences_lineaires(.)
  }) %>%
  ungroup()

prevalences_diplome_vieux_ensemble <- adultes_periodes %>%
  group_by(Sexe, periode) %>%
  do({
    prevalences_lineaires(.)
  }) %>%
  ungroup() %>%
  mutate(diplome = "Ensemble")

prevalences_diplome <- rbind(prevalences_diplome, prevalences_diplome_ensemble, prevalences_diplome_vieux, prevalences_diplome_vieux_ensemble) %>%
  filter(!is.na(diplome))%>%
  arrange(periode, Sexe, diplome, AGE)

saveRDS(prevalences_SL, "./interm/prevalences_SL.rds")
saveRDS(prevalences_PCS, "./interm/prevalences_PCS.rds")
saveRDS(prevalences_diplome, "./interm/prevalences_diplome.rds")
# ggplot(prevalences_diplome, aes(x = AGE, y = retraite_limite + non_retraite_limite,color=periode))+geom_line()+facet_grid(diplome~Sexe)