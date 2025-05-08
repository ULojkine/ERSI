# Décomposition de l'écart d'EVSI entre deux groupes
decomposition_EVSI <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_non_limite[elements1]) - sum(df$survie_non_limite[elements2]),
    survie = sum((df$survie[elements1] - df$survie[elements2])*df$non_limite[elements2]), # Calcul de la différence d'EVSI expliquée par la survie
    sante = sum(df$survie[elements2]*(df$non_limite[elements1] - df$non_limite[elements2])) # diff d'EVSI expliquée par la santé
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

decomposition_EVSIF <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_non_limite_forte[elements1]) - sum(df$survie_non_limite_forte[elements2]),
    survie = sum((df$survie[elements1] - df$survie[elements2])*df$non_limite_forte[elements2]), # Calcul de la différence d'EVSI expliquée par la survie
    sante = sum(df$survie[elements2]*(df$non_limite_forte[elements1] - df$non_limite_forte[elements2])) # diff d'EVSI expliquée par la santé
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}


# Fonction de décomposition de l'écart d'ER entre deux groupes
decomposition_ER <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_retraite[elements1]) - sum(df$survie_retraite[elements2]),
    survie = sum((df$survie[elements1] - df$survie[elements2])*df$retraite[elements2]), # Calcul de la différence d'ER expliquée par la survie
    retraite = sum(df$survie[elements2]*(df$retraite[elements1] - df$retraite[elements2])) # diff d'ER expliquée par la retraite
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}


# Fonction de décomposition de l'écart d'ERSI entre deux groupes
decomposition_ERSI <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la categorie examinée, elements2 = liste des indices pour la catégorie de référence
  composants <- c(total = sum(df$survie_retraite_non_limite[elements1]) - sum(df$survie_retraite_non_limite[elements2]),
                  survie = sum((df$survie[elements1]-df$survie[elements2])*df$retraite_non_limite[elements2]),
                  retraite = sum(df$survie[elements2]*df$non_limite_parmi_retraite[elements2]* (df$retraite[elements1] - df$retraite[elements2]), na.rm=T),
                  sante = sum(df$survie[elements2]*(df$non_limite_parmi_retraite[elements1] - df$non_limite_parmi_retraite[elements2])* df$retraite[elements2], na.rm=T)
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

# Décomposition de l'écart d'espérance de retraite sans incapacité forte
decomposition_ERSIF <- function(df, elements1, elements2){
  composants <-  c(total = sum(df$survie_retraite_non_limite_forte[elements1]) - sum(df$survie_retraite_non_limite_forte[elements2]),
                   survie = sum((df$survie[elements1]-df$survie[elements2])*df$retraite_non_limite_forte[elements2]),
                   retraite = sum(df$survie[elements2]*df$non_limite_forte_parmi_retraite[elements2]* (df$retraite[elements1] - df$retraite[elements2]), na.rm=T),
                   sante = sum(df$survie[elements2]*(df$non_limite_forte_parmi_retraite[elements1] - df$non_limite_forte_parmi_retraite[elements2])* df$retraite[elements2], na.rm=T)
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

calcul_esperances <- function(df, elements1, elements2){
  decomp_evsi <- decomposition_EVSI(df, elements1, elements2)
  decomp_evsif <- decomposition_EVSIF(df, elements1, elements2)
  decomp_er <- decomposition_ER(df,elements1,elements2)
  decomp_ersi <- decomposition_ERSI(df, elements1, elements2)
  decomp_ersif <- decomposition_ERSIF(df, elements1, elements2)
  
  # On construit une ligne prête à être ajoutée à un dataframe
  data.frame(
    ev = sum(df$survie[elements1]), # Calcul de l'EV
    survie60 = df$survie[elements1[60-30+1]],
    evsi = sum(df$survie_non_limite[elements1]), # Calcul de l'EVSI
    evsif = sum(df$survie_non_limite_forte[elements1]),
    survie60_non_limite = df$survie_non_limite[elements1[60-30+1]],
    survie60_non_limite_forte = df$survie_non_limite_forte[elements1[60-30+1]],
    er = sum(df$survie_retraite[elements1]), # ER
    ersi = sum(df$survie_retraite_non_limite[elements1]), #ERSI
    age_depart = sum(1 - df$retraite[elements1]), # Âge conjoncturel de départ 
    duree_retraite_naive = sum(df$survie[elements1]) - sum(1 - df$retraite[elements1]), # Indicateur usuel de durée de retraite : Espérance de vie - Âge conjoncturel de départ
    diff_ev = sum(df$survie[elements1]) - sum(df$survie[elements2]), # diff d'EV avec la référence
    diff_evsi = decomp_evsi["total"], # diff d'EVSI avec la référence
    diff_evsi_survie = decomp_evsi["survie"],
    diff_evsi_sante = decomp_evsi["sante"],
    diff_evsi_residu = decomp_evsi["residu"],
    diff_evsif = decomp_evsif["total"], # diff d'EVSIF avec la référence
    diff_evsif_survie = decomp_evsif["survie"],
    diff_evsif_sante = decomp_evsif["sante"],
    diff_evsif_residu = decomp_evsif["residu"],
    diff_er = decomp_er["total"], # diff d'ER avec la référence
    diff_er_survie = decomp_er["survie"], # Calcul de la différence d'ER avec la réf expliquée par la survie
    diff_er_retraite = decomp_er["retraite"], # diff d'ER expliquée par la retraite
    diff_er_residu = decomp_er["residu"],
    diff_ersi = decomp_ersi["total"], # diff d'ERSI avec la réf
    diff_ersi_survie = decomp_ersi["survie"],
    diff_ersi_retraite = decomp_ersi["retraite"],
    diff_ersi_sante = decomp_ersi["sante"],
    diff_ersi_residu = decomp_ersi["residu"],
    ersif = sum(df$survie_retraite_non_limite_forte[elements1]), #ERSI forte
    diff_ersif = decomp_ersif["total"],
    diff_ersif_survie = decomp_ersif["survie"],
    diff_ersif_retraite = decomp_ersif["retraite"],
    diff_ersif_sante = decomp_ersif["sante"],
    diff_ersif_residu = decomp_ersif["residu"]
  )
}

# On importe les données
for(enquete in c("SRCV","enqEmploi")){
  prevalences_survie_PCS <- readRDS(paste0("./interm/prevalences_survie_PCS_",enquete,".rds"))
  prevalences_survie_diplome <- readRDS(paste0("./interm/prevalences_survie_diplome_",enquete,".rds"))
  prevalences_survie_SL <- readRDS(paste0("./interm/prevalences_survie_SL_",enquete,".rds"))
  
  # On prépare les niveaux de factorisation
  sexe_levels <- c("Femmes","Hommes")
  periode_levels <- c("2009-2013","2017-2019")
  pcs_levels <- c("Cadres","Prof inter","Employés","Ouvriers","Ensemble")
  diplome_levels <- c("Supérieur","Bac","CAP","Brevet","Sans","Ensemble")
  
  factoriser <- function(df, colonnes){
    if("sexe" %in% colonnes){
      df <- mutate(df, Sexe = factor(Sexe, levels = sexe_levels))
    }
    if("periode" %in% colonnes){
      df <- mutate(df, periode = factor(periode, levels = periode_levels))
    }
    if("PCS" %in% colonnes){
      df <- mutate(df, PCS = factor(PCS, levels = pcs_levels))
    }
    if("diplome" %in% colonnes){
      df <- mutate(df, diplome = factor(diplome, levels = diplome_levels))
    }
    return(df)
  }
  
  ## COMPARAISON ENTRE SEXES POUR CHAQUE PÉRIODE
  comparaison_entre_sexes <- expand_grid(sexe = sexe_levels, periode1 = periode_levels) %>%
    pmap_dfr(function(sexe, periode1){
      p <- prevalences_survie_PCS %>% filter(periode == periode1, PCS == "Ensemble")
      calcul_esperances(p, which(p$Sexe == sexe), which(p$Sexe == "Hommes"))%>%
        mutate(periode = periode1,
               Sexe = sexe)
    }) %>%
    select(periode, Sexe, everything()) %>%
    factoriser(c("periode","Sexe"))%>%
    remove_rownames()
  
  ## COMPARAISON ENTRE PCS POUR CHAQUE SEXE ET CHAQUE PÉRIODE
  comparaison_entre_PCS <- expand_grid(sexe = sexe_levels, periode1 = periode_levels, pcs = pcs_levels) %>%
                            pmap_dfr(function(sexe, periode1, pcs){
                                         p <- prevalences_survie_PCS %>% filter(Sexe == sexe, periode == periode1)
                                         calcul_esperances(p, which(p$PCS == pcs), which(p$PCS == "Cadres"))%>%
                                           mutate(periode = periode1,
                                                  Sexe = sexe,
                                                  PCS = pcs)
                            }) %>%
    select(periode, Sexe, PCS, everything()) %>%
    factoriser(c("periode","Sexe","PCS"))%>%
    remove_rownames()
  
  ## COMPARAISON ENTRE DIPLÔMES POUR CHAQUE SEXE ET CHAQUE PÉRIODE
  comparaison_entre_diplomes <- expand_grid(sexe = sexe_levels, periode1 = periode_levels, diplome1 =diplome_levels) %>%
    pmap_dfr(function(sexe, periode1, diplome1){
      p <- prevalences_survie_diplome %>% filter(Sexe == sexe, periode == periode1)
      calcul_esperances(p, which(p$diplome == diplome1), which(p$diplome == "Supérieur"))%>%
        mutate(periode = periode1,
               Sexe = sexe,
               diplome = diplome1)
    }) %>%
    select(periode, Sexe, diplome, everything()) %>%
    factoriser(c("periode","Sexe","diplome"))%>%
    remove_rownames()
  
  ## COMPARAISON ENTRE PÉRIODES PAR PCS ET SEXE
  comparaison_entre_periodes_par_PCS <- expand_grid(sexe = sexe_levels, periode1 = periode_levels, pcs = pcs_levels) %>%
    pmap_dfr(function(sexe, periode1, pcs){
      p <- prevalences_survie_PCS %>% filter(Sexe == sexe, PCS == pcs)
      calcul_esperances(p, which(p$periode == periode1), which(p$periode == "2009-2013"))%>%
        mutate(periode = periode1,
               Sexe = sexe,
               PCS = pcs)
    }) %>%
    select(periode, Sexe, PCS, everything()) %>%
    factoriser(c("periode","Sexe","PCS"))%>%
    remove_rownames()
  
  ## COMPARAISON ENTRE PÉRIODES PAR DIPLÔME ET SEXE
  comparaison_entre_periodes_par_diplome <- expand_grid(sexe = sexe_levels, periode1 = periode_levels, diplome1 = diplome_levels) %>%
    pmap_dfr(function(sexe, periode1, diplome1){
      p <- prevalences_survie_diplome %>% filter(Sexe == sexe, diplome == diplome1)
      calcul_esperances(p, which(p$periode == periode1), which(p$periode == "2009-2013"))%>%
        mutate(periode = periode1,
               Sexe = sexe,
               diplome = diplome1)
    }) %>%
    select(periode, Sexe, diplome, everything()) %>%
    factoriser(c("periode","Sexe","diplome"))%>%
    remove_rownames()
  
  ## COMPARAISON ENTRE ANNÉES (SÉRIE LONGUE), PAR SEXE
  annee_min <- case_when(enquete == "SRCV" ~ 2008,
                         enquete == "enqEmploi" ~ 2013) # La série SRCV remonte jusqu'à 2008, mais EE seulement jusqu'à 2013
  comparaison_entre_annees <- expand_grid(sexe = sexe_levels, annee = c(annee_min:2019)) %>%
                            pmap_dfr( function(sexe, annee){
                              p <- prevalences_survie_SL %>% filter(Sexe == sexe)
                              calcul_esperances(p, which(p$AENQ == annee), which(p$AENQ == annee_min))%>%
                                mutate(AENQ = annee,
                                       Sexe = sexe)
                            }) %>%
    select(AENQ, Sexe, everything()) %>%
    factoriser(c("Sexe"))%>%
    remove_rownames()
  
  # Export
  saveRDS(comparaison_entre_sexes, paste0("interm/comparaison_entre_sexes_",enquete,".rds"))
  saveRDS(comparaison_entre_PCS, paste0("interm/comparaison_entre_PCS_",enquete,".rds"))
  saveRDS(comparaison_entre_diplomes, paste0("interm/comparaison_entre_diplomes_",enquete,".rds"))
  saveRDS(comparaison_entre_periodes_par_PCS, paste0("interm/comparaison_entre_periodes_par_PCS_",enquete,".rds"))
  saveRDS(comparaison_entre_periodes_par_diplome, paste0("interm/comparaison_entre_periodes_par_diplome_",enquete,".rds"))
  saveRDS(comparaison_entre_annees, paste0("interm/comparaison_entre_annees_",enquete,".rds"))
  
  write_csv(comparaison_entre_sexes, paste0("sorties/comparaison_entre_sexes_",enquete,".csv"))
  write_csv(comparaison_entre_PCS, paste0("sorties/comparaison_entre_PCS_",enquete,".csv"))
  write_csv(comparaison_entre_diplomes, paste0("sorties/comparaison_entre_diplomes_",enquete,".csv"))
  write_csv(comparaison_entre_periodes_par_PCS, paste0("sorties/comparaison_entre_periodes_par_PCS_",enquete,".csv"))
  write_csv(comparaison_entre_periodes_par_diplome, paste0("sorties/comparaison_entre_periodes_par_diplome_",enquete,".csv"))
  write_csv(comparaison_entre_annees, paste0("sorties/comparaison_entre_annees_",enquete,".csv"))
}