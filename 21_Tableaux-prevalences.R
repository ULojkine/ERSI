# Fonctions ####
seuils_agecat <- c(30+5*(0:11), Inf)
labels_agecat <- c(30+5*(0:10)) %>% paste0(.,"-",.+4) %>% c(.,"85+")

categoriser_ages <- function(df){
  df %>% mutate(
    agecat = cut(AGE, breaks = seuils_agecat, labels = labels_agecat, right=FALSE),
    agecat_nombre = as.integer(substr(agecat, 1, 2))
  )
}


# Calcul de la prévalence moyenne par catégorie d'âge
prevalences_moyennes_agecat <- function(donnees, ...) {
  donnees %>% 
    filter(AGE %in% c(30:100)) %>% 
    group_by(agecat_nombre, ...) %>%
    rename(
      retraite_indiv = retraite
    ) %>%
    summarise(
      non_limite = weighted_mean_na_rm(!limite, PB040),
      non_limite_var = weighted_var_na_rm(!limite, PB040),
      non_limite_forte = weighted_mean_na_rm(!limite_forte, PB040),
      non_limite_forte_var = weighted_var_na_rm(!limite_forte, PB040),
      retraite = weighted_mean_na_rm(retraite_indiv, PB040),
      retraite_var = weighted_var_na_rm(retraite_indiv, PB040),
      retraite_non_limite = weighted_mean_na_rm(retraite_indiv & !limite, PB040),
      retraite_non_limite_var = weighted_var_na_rm(retraite_indiv & !limite, PB040),
      retraite_non_limite_forte = weighted_mean_na_rm(retraite_indiv & !limite_forte, PB040),
      retraite_non_limite_forte_var = weighted_var_na_rm(retraite_indiv & !limite_forte, PB040),
      non_retraite_non_limite = weighted_mean_na_rm(!retraite_indiv & !limite, PB040),
      non_retraite_limite = weighted_mean_na_rm(!retraite_indiv & limite, PB040),
      non_retraite_non_limite_forte = weighted_mean_na_rm(!retraite_indiv & !limite_forte, PB040),
      non_limite_parmi_retraite = ifelse(retraite == 0, 1, retraite_non_limite/retraite),
      non_limite_forte_parmi_retraite = ifelse(retraite == 0, 1, retraite_non_limite_forte/retraite),
      .groups='drop'
    ) %>%
    uncount(ifelse(agecat_nombre < 85, 5, 16), .id = "id_clone") %>%  # Creates 5 copies with counter
    mutate(AGE = agecat_nombre + id_clone - 1) %>% 
    select(-id_clone) %>%
    ungroup()
}

# Sur le même modèle, on calcule les proportions entre groupes 
proportions_agecat <- function(donnees, critere = "PCS") {
  donnees %>% 
    filter(AGE %in% c(30:100)) %>%
    group_by(periode, Sexe, agecat_nombre, !!sym(critere)) %>%
    summarise(
      poids_groupe = sum(PB040)
    ) %>%
    group_by(periode,Sexe,agecat_nombre) %>%
    mutate(
      proportion = poids_groupe / sum(poids_groupe) 
    ) %>%
    uncount(ifelse(agecat_nombre < 85, 5, 16), .id = "id_clone") %>%  # Creates 5 copies with counter
    mutate(AGE = agecat_nombre + id_clone - 1) %>% 
    select(-c(id_clone, poids_groupe)) %>%
    ungroup()
}

# et entre sexes, dans un groupe donné
proportions_agecat_parSexe <- function(donnees, critere = "PCS") {
  donnees %>% 
    filter(AGE %in% c(30:100)) %>%
    group_by(periode, Sexe, agecat_nombre, !!sym(critere)) %>%
    summarise(
      poids_groupe = sum(PB040)
    ) %>%
    group_by(periode,!!sym(critere),agecat_nombre) %>%
    mutate(
      proportion = poids_groupe / sum(poids_groupe) 
    ) %>%
    uncount(ifelse(agecat_nombre < 85, 5, 16), .id = "id_clone") %>%  # Creates 5 copies with counter
    mutate(AGE = agecat_nombre + id_clone - 1) %>% 
    select(-c(id_clone, poids_groupe)) %>%
    ungroup()
}


# Spécification alternative : moyenne glissante
prevalences_moyenne_glissante <- function(donnees, age, ...) {
  donnees %>% 
    filter(AGE >= age-2 & AGE <= age+2) %>% 
    group_by(...) %>%
    rename(
      retraite_indiv = retraite
    ) %>%
    summarise(
      AGE = age,
      non_limite = weighted_mean_na_rm(!limite, PB040),
      non_limite_var = weighted_var_na_rm(!limite, PB040),
      non_limite_forte = weighted_mean_na_rm(!limite_forte, PB040),
      non_limite_forte_var = weighted_var_na_rm(!limite_forte, PB040),
      retraite = weighted_mean_na_rm(retraite_indiv, PB040),
      retraite_var = weighted_var_na_rm(retraite_indiv,PB040),
      retraite_non_limite = weighted_mean_na_rm(retraite_indiv & !limite, PB040),
      retraite_non_limite_var = weighted_var_na_rm(retraite_indiv & !limite, PB040),
      retraite_non_limite_forte = weighted_mean_na_rm(retraite_indiv & !limite_forte, PB040),
      retraite_non_limite_forte_var = weighted_var_na_rm(retraite_indiv & !limite_forte, PB040),
      non_retraite_non_limite = weighted_mean_na_rm(!retraite_indiv & !limite, PB040),
      non_retraite_limite = weighted_mean_na_rm(!retraite_indiv & limite, PB040),
      non_retraite_non_limite_forte = weighted_mean_na_rm(!retraite_indiv & !limite_forte, PB040),
      non_limite_parmi_retraite = ifelse(retraite == 0, 1, retraite_non_limite/retraite),
      non_limite_forte_parmi_retraite = ifelse(retraite == 0, 1, retraite_non_limite_forte/retraite),
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
prevalences_lineaires <- function(donnees) {
  ages_avances <- data.frame(AGE = 75:100)
  
  # Prédictions avec erreur standard pour limite
  pred_limite <- predict(modele_lineaire(donnees, "limite"), newdata = ages_avances, se.fit = TRUE)
  
  # Prédictions avec erreur standard pour limite_forte
  pred_limite_forte <- predict(modele_lineaire(donnees, "limite_forte"), newdata = ages_avances, se.fit = TRUE)
  
  # Calcul des valeurs prédites et variances
  ages_avances %>%
    mutate(
      non_limite = 1 - pmin(1, pred_limite$fit),
      non_limite_var = pred_limite$se.fit^2,
      non_limite_forte = 1 - pmin(1, pred_limite_forte$fit),
      non_limite_forte_var = pred_limite_forte$se.fit^2,
      retraite = 1,
      retraite_var = 0,
      retraite_non_limite = non_limite,
      retraite_non_limite_var = non_limite_var,
      retraite_non_limite_forte = non_limite_forte,
      retraite_non_limite_forte_var = non_limite_forte_var,
      non_retraite_non_limite = 0,
      non_retraite_limite = 0,
      non_retraite_non_limite_forte = 0,
      non_limite_parmi_retraite = ifelse(retraite == 0, 1, retraite_non_limite/retraite),
      non_limite_forte_parmi_retraite = ifelse(retraite == 0, 1, retraite_non_limite_forte/retraite),
    )
}

# Calcul des prévalences pour chaque variante ####
for(variante in liste_variantes[liste_variantes != "SRCV_recensement"]){
  print(variante)
  enquete <- ifelse(startsWith(variante, "SRCV"), "SRCV","enqEmploi")
  adultes <- readRDS(paste0("./interm/adultes_",enquete,".rds"))
  if(endsWith(variante,"revenu")){  # variante où la retraite est définie par le fait de toucher une pension en t-1 et non de manière déclarative
      adultes <- mutate(adultes, retraite = retraite_revenu_i)
  }
  
  if(endsWith(variante,"retraitebrut")){
    adultes <- mutate(adultes, retraite = retraite_brut)
  }
  
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
  
  if(endsWith(variante,"lisse")){   ## Variante lissée ####
    # TABLE PAR PCS, PERIODE, LISSAGE LINÉAIRE
    prevalences_PCS <- map_dfr(c(30:74), ~prevalences_moyenne_glissante(donnees = filter(adultes_periodes, PCS %notin% c("Agriculteurs","Artisans",NA,"Inactifs")),
                                                             age = ., Sexe, PCS, periode) )
    
    prevalences_PCS_ensemble <- map_dfr(c(30:74), ~prevalences_moyenne_glissante(donnees = adultes_periodes,
                                                                      age = ., Sexe, periode) ) %>%
    mutate(PCS = "Ensemble")
    
    # Les agriculteurs et les artisans ne sont pas assez nombreux, on regroupe les deux sexes
    prevalences_PCS_agriculteurs_artisans <- map_dfr(c(30:74), ~prevalences_moyenne_glissante(donnees = filter(adultes_periodes, PCS %in% c("Agriculteurs","Artisans")),
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
    
    
    # TABLE PAR DIPLÔME, PERIODE, LISSAGE LINÉAIRE
    prevalences_diplome <- map_dfr(c(30:74), ~prevalences_moyenne_glissante(donnees = adultes_periodes, age = ., Sexe, diplome, periode) )
    
    prevalences_diplome_bacOuMoins <- map_dfr(c(30:74), ~prevalences_moyenne_glissante(donnees = filter(adultes_periodes, diplome != "Supérieur"),
                                                                            age = ., Sexe, periode) ) %>%
      mutate(diplome = "Bac ou moins")
    
    
    prevalences_diplome_ensemble <- map_dfr(c(30:74), ~prevalences_moyenne_glissante(donnees = adultes_periodes,
                                                                      age = ., Sexe, periode) ) %>%
    mutate(diplome = "Ensemble")
    
    prevalences_diplome_vieux <- adultes_periodes %>%
    filter(!is.na(diplome)) %>%
    group_by(Sexe, diplome, periode) %>%
    do({
      prevalences_lineaires(.)
    }) %>%
    ungroup()
    
    prevalences_diplome_vieux_bacOuMoins <- adultes_periodes %>%
      filter(diplome != "Supérieur") %>%
      group_by(Sexe, periode) %>%
      do({
        prevalences_lineaires(.)
      }) %>%
      ungroup() %>%
      mutate(diplome = "Bac ou moins")
    
    
    prevalences_diplome_vieux_ensemble <- adultes_periodes %>%
    group_by(Sexe, periode) %>%
    do({
      prevalences_lineaires(.)
    }) %>%
    ungroup() %>%
    mutate(diplome = "Ensemble")
    
    prevalences_diplome <- rbind(prevalences_diplome, prevalences_diplome_bacOuMoins, prevalences_diplome_ensemble, prevalences_diplome_vieux, prevalences_diplome_vieux_bacOuMoins, prevalences_diplome_vieux_ensemble) %>%
    filter(!is.na(diplome))%>%
    arrange(periode, Sexe, diplome, AGE)
  } else{
  ##Autres variantes ####
    adultes <- categoriser_ages(adultes)
    adultes_periodes <- categoriser_ages(adultes_periodes)

    # Par PCS par période
    prevalences_PCS <- prevalences_moyennes_agecat(donnees = filter(adultes_periodes, PCS %notin% c("Agriculteurs","Artisans",NA,"Inactifs")), Sexe, PCS, periode)
    
    prevalences_PCS_ensemble <- prevalences_moyennes_agecat(donnees = adultes_periodes,Sexe, periode) %>%
      mutate(PCS = "Ensemble")
    
    prevalences_PCS_tousSexes <- prevalences_moyennes_agecat(donnees = filter(adultes_periodes, !is.na(PCS), PCS %notin% c(NA, "Inactifs")), PCS, periode) %>%
      mutate(Sexe =  "Ensemble")
    
    prevalences_PCS <- rbind(prevalences_PCS, prevalences_PCS_ensemble, prevalences_PCS_tousSexes) %>%
      arrange(periode, Sexe, PCS, AGE)
    
    # Par diplôme et période
    prevalences_diplome <- prevalences_moyennes_agecat(donnees = adultes_periodes, Sexe, diplome, periode)
    prevalences_diplome_bacOuMoins <- prevalences_moyennes_agecat(donnees = filter(adultes_periodes, diplome != "Supérieur"), Sexe, periode) %>%
      mutate(diplome = "Bac ou moins")
    prevalences_diplome_ensemble <- prevalences_moyennes_agecat(donnees = adultes_periodes, Sexe, periode) %>%
      mutate(diplome = "Ensemble")
    prevalences_diplome_tousSexes <- prevalences_moyennes_agecat(donnees = adultes_periodes, diplome, periode) %>%
      mutate(Sexe = "Ensemble")
    
    prevalences_diplome <- rbind(prevalences_diplome, prevalences_diplome_bacOuMoins, prevalences_diplome_ensemble, prevalences_diplome_tousSexes) %>%
      filter(!is.na(diplome))%>%
      arrange(periode, Sexe, diplome, AGE)
    
  }
  
  saveRDS(adultes_periodes, paste0("./interm/adultes_periodes_",variante,".rds"))
  saveRDS(prevalences_PCS, paste0("./interm/prevalences_PCS_",variante,".rds"))
  saveRDS(prevalences_diplome, paste0("./interm/prevalences_diplome_",variante,".rds"))
  
  if(variante == "SRCV"){ # tant qu'on est dans la spécification de base, on se livre à deux exercices : calculer les proportions de chaque catégorie, et prendre en compte les EHPAD via le recensement
    ### Proportions des catégories ####
    proportions_PCS <- proportions_agecat(adultes_periodes, critere = "PCS")
    proportions_PCS_parSexe <- proportions_agecat_parSexe(adultes_periodes, critere = "PCS")
    proportions_diplome <- proportions_agecat(adultes_periodes, critere = "diplome")
    proportions_diplome_parSexe <- proportions_agecat_parSexe(adultes_periodes, critere = "diplome")
    
    saveRDS(proportions_PCS, "./interm/proportions_PCS.rds")
    saveRDS(proportions_PCS_parSexe, "./interm/proportions_PCS_parSexe.rds")
    saveRDS(proportions_diplome, "./interm/proportions_diplome.rds")
    saveRDS(proportions_diplome_parSexe, "./interm/proportions_diplome_parSexe.rds")
    
    ### Prise en compte du recensement ####
    recensement2019 <- read_delim("./data/Recensement/comptage_DIPL_AGED_ordinaires_2019.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
    
    recensement2019 <- recensement2019 %>%
      rename(
        AGE = AGED,
        Sexe = SEXE,
        diplome = diplome_regroupe
      ) %>%
      mutate(
        AGE = as.integer(AGE),
        Sexe = case_when(
          Sexe == 1 ~ "Hommes",
          Sexe == 2 ~ "Femmes"
        ),
        diplome = sub("^0\\w+ - ","",diplome)
      )
    
    # On complète les lignes manquantes
    recensement2019 <- recensement2019 %>%
      filter(AGE %in% c(30:100)) %>%
      complete(diplome,Sexe,AGE, fill = list(ordinaires = 0, non_ordinaires = 0))
    
    # Catégories agrégées
    recensement2019_bacOuMoins <- recensement2019 %>%
      filter(diplome != "Supérieur") %>%
      group_by(Sexe,AGE) %>%
      summarise(
        ordinaires = sum(ordinaires),
        non_ordinaires = sum(non_ordinaires),
        nobs_echant = sum(nobs_echant),
        prop=NA,
        diplome = "Bac ou moins"
      )
    
    recensement2019_ensemble <- recensement2019 %>%
      group_by(Sexe,AGE) %>%
      summarise(
        ordinaires = sum(ordinaires),
        non_ordinaires = sum(non_ordinaires),
        nobs_echant = sum(nobs_echant),
        prop=NA,
        diplome = "Ensemble"
      )
    
    recensement2019 <- rbind(recensement2019,
                             recensement2019_bacOuMoins,
                             recensement2019_ensemble)
    
    recensement2019_cat <- recensement2019 %>%
      categoriser_ages %>%
      group_by(agecat_nombre,diplome,Sexe)%>%
      mutate(
        proportion_ordinaires = sum(ordinaires)/sum(ordinaires+non_ordinaires),
        proportion_ordinaires_var = proportion_ordinaires*(1-proportion_ordinaires)/sum(nobs_echant)
      ) %>%
      ungroup()%>%
      select(AGE,diplome,Sexe,proportion_ordinaires,proportion_ordinaires_var) %>%
      filter(AGE %in% c(70:100))
    
    prevalences_diplome_recensement <- prevalences_diplome %>%
      filter(periode == "2017-2019", AGE %in% c(70:100)) %>%
      left_join(., recensement2019_cat, by=c("diplome","Sexe","AGE")) %>%
      rename(
        non_limite_brut = non_limite,
        non_limite_brut_var = non_limite_var,
        non_limite_forte_brut = non_limite_forte,
        non_limite_forte_brut_var = non_limite_forte_var
      )%>%
      mutate(
        non_limite = non_limite_brut*proportion_ordinaires,
        non_limite_var = non_limite_brut^2*proportion_ordinaires_var+proportion_ordinaires^2*non_limite_brut_var+proportion_ordinaires_var*non_limite_brut_var, # V(XY) = V(X)E(Y)2 + V(Y)E(X)2 + V(X)V(Y)
        non_limite_forte = non_limite_forte_brut*proportion_ordinaires,
        non_limite_forte_var = non_limite_forte_brut^2*proportion_ordinaires_var+proportion_ordinaires^2*non_limite_forte_brut_var+proportion_ordinaires_var*non_limite_forte_brut_var,
        retraite_non_limite = non_limite,
        retraite_non_limite_var = non_limite_var,
        retraite_non_limite_forte = non_limite_forte,
        retraite_non_limite_forte_var = non_limite_forte_var,
        non_retraite_non_limite = 0,
        non_retraite_limite = 0,
        non_retraite_non_limite_forte = 0,
        non_limite_parmi_retraite = non_limite,
        non_limite_forte_parmi_retraite = non_limite_forte,
      ) %>%
      select(
        -c(proportion_ordinaires, proportion_ordinaires_var,non_limite_brut, non_limite_brut_var,non_limite_forte_brut,non_limite_forte_brut_var)
      )
    
    colnames(prevalences_diplome_recensement)
    
    prevalences_diplome_recensement <- prevalences_diplome %>%
      filter(periode == "2017-2019", AGE < 70) %>%
      rbind(prevalences_diplome_recensement) %>%
      arrange(Sexe,diplome,AGE)
    
    saveRDS(prevalences_diplome_recensement, paste0("./interm/prevalences_diplome_SRCV_recensement.rds"))
  }
  rm(adultes)
  gc()
}