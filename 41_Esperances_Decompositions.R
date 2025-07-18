# Factorisation
# On prépare les niveaux de factorisation
sexe_levels <- c("Femmes","Hommes","Ensemble")
periode_levels <- c("2009-2013","2017-2019")
pcs_levels <- c("Cadres","Prof inter","Employés","Ouvriers","Ensemble")
diplome_levels <- c("Supérieur","Bac","CAP","Brevet","Sans","Bac ou moins","Ensemble")

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

# Décomposition de l'écart d'EVSI entre deux groupes
decomposition_EVSI <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_non_limite[elements1]) - sum(df$survie_non_limite[elements2]),
    survie = sum((df$survie_ap[elements1] - df$survie_ap[elements2])*df$non_limite[elements2]), # Calcul de la différence d'EVSI expliquée par la survie
    sante = sum(df$survie_ap[elements2]*(df$non_limite[elements1] - df$non_limite[elements2])) # diff d'EVSI expliquée par la santé
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

decomposition_EVSIF <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_non_limite_forte[elements1]) - sum(df$survie_non_limite_forte[elements2]),
    survie = sum((df$survie_ap[elements1] - df$survie_ap[elements2])*df$non_limite_forte[elements2]), # Calcul de la différence d'EVSI expliquée par la survie
    sante = sum(df$survie_ap[elements2]*(df$non_limite_forte[elements1] - df$non_limite_forte[elements2])) # diff d'EVSI expliquée par la santé
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}


# Fonction de décomposition de l'écart d'ER entre deux groupes
decomposition_ER <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_retraite[elements1]) - sum(df$survie_retraite[elements2]),
    survie = sum((df$survie_ap[elements1] - df$survie_ap[elements2])*df$retraite[elements2]), # Calcul de la différence d'ER expliquée par la survie
    retraite = sum(df$survie_ap[elements2]*(df$retraite[elements1] - df$retraite[elements2])) # diff d'ER expliquée par la retraite
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}


# Fonction de décomposition de l'écart d'ERSI entre deux groupes
decomposition_ERSI <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la categorie examinée, elements2 = liste des indices pour la catégorie de référence
  composants <- c(total = sum(df$survie_retraite_non_limite[elements1]) - sum(df$survie_retraite_non_limite[elements2]),
                  survie = sum((df$survie_ap[elements1]-df$survie_ap[elements2])*df$retraite_non_limite[elements2]),
                  retraite = sum(df$survie_ap[elements2]*df$non_limite_parmi_retraite[elements2]* (df$retraite[elements1] - df$retraite[elements2]), na.rm=T),
                  sante = sum(df$survie_ap[elements2]*(df$non_limite_parmi_retraite[elements1] - df$non_limite_parmi_retraite[elements2])* df$retraite[elements2], na.rm=T)
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

# Décomposition de l'écart d'espérance de retraite sans incapacité forte
decomposition_ERSIF <- function(df, elements1, elements2){
  composants <-  c(total = sum(df$survie_retraite_non_limite_forte[elements1]) - sum(df$survie_retraite_non_limite_forte[elements2]),
                   survie = sum((df$survie_ap[elements1]-df$survie_ap[elements2])*df$retraite_non_limite_forte[elements2]),
                   retraite = sum(df$survie_ap[elements2]*df$non_limite_forte_parmi_retraite[elements2]* (df$retraite[elements1] - df$retraite[elements2]), na.rm=T),
                   sante = sum(df$survie_ap[elements2]*(df$non_limite_forte_parmi_retraite[elements1] - df$non_limite_forte_parmi_retraite[elements2])* df$retraite[elements2], na.rm=T)
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

var_esperance <- function(df, elements, nom_colonne){
  df_var <- df[elements,]
  nom_var <- paste0(nom_colonne, "_var")
  df_var <- df_var %>%
    group_by(agecat_nombre) %>%
    summarise(
      survie_ap = sum(survie_ap),
      var_cat = first(.data[[nom_var]]) #toutes les vars sont identiques
    )
  
  return(sum(df_var$survie_ap^2*df_var$var_cat))
}

# Variance de la différence entre deux espérances
var_diff_esperance <- function(df, elements1, elements2, nom_colonne){
  if(identical(elements1,elements2)){
    return(0)
  } else{
    return(var_esperance(df, elements1, nom_colonne) + var_esperance(df, elements2, nom_colonne))
  }
}

calcul_esperances <- function(df, elements1, elements2, variante){
  decomp_evsi <- decomposition_EVSI(df, elements1, elements2)
  decomp_evsif <- decomposition_EVSIF(df, elements1, elements2)
  decomp_er <- decomposition_ER(df,elements1,elements2)
  decomp_ersi <- decomposition_ERSI(df, elements1, elements2)
  decomp_ersif <- decomposition_ERSIF(df, elements1, elements2)
  
  # On construit une ligne prête à être ajoutée à un dataframe
  ligne <- data.frame(
    ev = sum(df$survie_ap[elements1]), # Calcul de l'EV
    survie60 = df$survie[elements1[60-30+1]],
    evsi = sum(df$survie_non_limite[elements1]), # Calcul de l'EVSI
    evsi65 = sum(df$survie_non_limite[elements1[(65-29):(100-29)]]),
    evsif = sum(df$survie_non_limite_forte[elements1]),
    survie60_non_limite = df$survie_non_limite[elements1[60-30+1]],
    survie60_non_limite_forte = df$survie_non_limite_forte[elements1[60-30+1]],
    er = sum(df$survie_retraite[elements1]), # ER
    ersi = sum(df$survie_retraite_non_limite[elements1]), #ERSI
    age_depart = sum(1 - df$retraite[elements1]), # Âge conjoncturel de départ 
    duree_retraite_naive = sum(df$survie_ap[elements1]) - sum(1 - df$retraite[elements1]), # Indicateur usuel de durée de retraite : Espérance de vie - Âge conjoncturel de départ
    diff_ev = sum(df$survie_ap[elements1]) - sum(df$survie_ap[elements2]), # diff d'EV avec la référence
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
  
  for(type_esperance in c("evsi","evsif","er","ersi","ersif")){
    nom_colonne <- case_when(
      type_esperance == "evsi" ~ "non_limite",
      type_esperance == "evsif" ~ "non_limite_forte",
      type_esperance == "er" ~ "retraite",
      type_esperance == "ersi" ~ "retraite_non_limite",
      type_esperance == "ersif" ~ "retraite_non_limite_forte"
    )
    if(endsWith(variante,"lisse")){
      var_esp <- 0
      var_diff <- 0
    } else {
      var_esp <- var_esperance(df, elements1, nom_colonne)
      var_diff <- var_diff_esperance(df, elements1, elements2, nom_colonne)
    }
    ligne[[paste0(type_esperance, "_var")]] <- var_esp
    ligne[[paste0("diff_",type_esperance, "_var")]] <- var_diff
    ligne[[paste0("diff_",type_esperance,"_ci_inf")]] <- ligne[[paste0("diff_",type_esperance)]]-qnorm(0.975)*sqrt(var_diff)
    ligne[[paste0("diff_",type_esperance,"_ci_sup")]] <- ligne[[paste0("diff_",type_esperance)]]+qnorm(0.975)*sqrt(var_diff)
  }
  return(ligne)
}

# On importe les données
for(variante in liste_variantes){
  print(variante)
  if(endsWith(variante,"recensement")){
    periode_levels <- c("2017-2019")
  } else{
    periode_levels <- c("2009-2013","2017-2019")
  }
  prevalences_survie_diplome <- readRDS(paste0("./interm/prevalences_survie_diplome_",variante,".rds"))
  ## COMPARAISON ENTRE DIPLÔMES POUR CHAQUE SEXE ET CHAQUE PÉRIODE
  comparaison_entre_diplomes <- expand_grid(sexe = sexe_levels, periode1 = periode_levels, diplome1 =diplome_levels) %>%
    pmap_dfr(function(sexe, periode1, diplome1){
      p <- prevalences_survie_diplome %>% filter(Sexe == sexe, periode == periode1)
      calcul_esperances(p, which(p$diplome == diplome1), which(p$diplome == "Supérieur"),variante)%>%
        mutate(periode = periode1,
               Sexe = sexe,
               diplome = diplome1)
    }) %>%
    select(periode, Sexe, diplome, everything()) %>%
    factoriser(c("periode","Sexe","diplome"))%>%
    remove_rownames()
  
  saveRDS(comparaison_entre_diplomes, paste0("interm/comparaison_entre_diplomes_",variante,".rds"))
  write_csv(comparaison_entre_diplomes, paste0("sorties/comparaison_entre_diplomes_",variante,".csv"))
  
  if(endsWith(variante,"recensement")){
    next
  }
  
  ## Évolution de l'écart entre catégories de diplôme
  colsASoustraire <- comparaison_entre_diplomes %>%
    select(matches("^diff_")) %>%
    select( -matches("var$"),
            -matches("ci_inf$"),
            -matches("ci_sup$")
    ) %>%
    colnames()
  
  colsAAdditionner <- comparaison_entre_diplomes %>%
    select(matches("^diff_.*var$")) %>%
    colnames()
  
  diff_diff <- comparaison_entre_diplomes %>%
    group_by(Sexe,diplome) %>%
    summarise(
      across(all_of(colsASoustraire), ~ .[periode == "2017-2019"] - .[periode == "2009-2013"]),
      across(all_of(colsAAdditionner),  ~ .[periode == "2017-2019"] + .[periode == "2009-2013"])
    )
  
  colsAvecVar <- gsub("_var$", "", colsAAdditionner)
  # Ajouter les colonnes ci_inf et ci_sup une à une
  for (col in colsAvecVar) {
    diff_diff[[paste0(col, "_ci_inf")]] <- diff_diff[[col]] - qnorm(0.975) * sqrt(diff_diff[[paste0(col, "_var")]])
    diff_diff[[paste0(col, "_ci_sup")]] <- diff_diff[[col]] + qnorm(0.975) * sqrt(diff_diff[[paste0(col, "_var")]])
  }
  
  prevalences_survie_PCS <- readRDS(paste0("./interm/prevalences_survie_PCS_",variante,".rds"))

  ## COMPARAISON ENTRE SEXES POUR CHAQUE PÉRIODE
  comparaison_entre_sexes <- expand_grid(sexe = sexe_levels, periode1 = periode_levels) %>%
    pmap_dfr(function(sexe, periode1){
      p <- prevalences_survie_PCS %>% filter(periode == periode1, PCS == "Ensemble")
      calcul_esperances(p, which(p$Sexe == sexe), which(p$Sexe == "Hommes"), variante)%>%
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
                                         calcul_esperances(p, which(p$PCS == pcs), which(p$PCS == "Cadres"),variante)%>%
                                           mutate(periode = periode1,
                                                  Sexe = sexe,
                                                  PCS = pcs)
                            }) %>%
    select(periode, Sexe, PCS, everything()) %>%
    factoriser(c("periode","Sexe","PCS"))%>%
    remove_rownames()
  
  ## COMPARAISON ENTRE PÉRIODES PAR PCS ET SEXE
  comparaison_entre_periodes_par_PCS <- expand_grid(sexe = sexe_levels, periode1 = periode_levels, pcs = pcs_levels) %>%
    pmap_dfr(function(sexe, periode1, pcs){
      p <- prevalences_survie_PCS %>% filter(Sexe == sexe, PCS == pcs)
      calcul_esperances(p, which(p$periode == periode1), which(p$periode == "2009-2013"),variante)%>%
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
      calcul_esperances(p, which(p$periode == periode1), which(p$periode == "2009-2013"),variante)%>%
        mutate(periode = periode1,
               Sexe = sexe,
               diplome = diplome1)
    }) %>%
    select(periode, Sexe, diplome, everything()) %>%
    factoriser(c("periode","Sexe","diplome"))%>%
    remove_rownames()
  
  
  # Export
  saveRDS(comparaison_entre_sexes, paste0("interm/comparaison_entre_sexes_",variante,".rds"))
  saveRDS(comparaison_entre_PCS, paste0("interm/comparaison_entre_PCS_",variante,".rds"))
  saveRDS(comparaison_entre_periodes_par_PCS, paste0("interm/comparaison_entre_periodes_par_PCS_",variante,".rds"))
  saveRDS(comparaison_entre_periodes_par_diplome, paste0("interm/comparaison_entre_periodes_par_diplome_",variante,".rds"))
  saveRDS(diff_diff, paste0("interm/comparaison_entre_periodes_ecarts_entre_diplomes_",variante,".rds"))
  
  write_csv(comparaison_entre_sexes, paste0("sorties/comparaison_entre_sexes_",variante,".csv"))
  write_csv(comparaison_entre_PCS, paste0("sorties/comparaison_entre_PCS_",variante,".csv"))
  write_csv(comparaison_entre_periodes_par_PCS, paste0("sorties/comparaison_entre_periodes_par_PCS_",variante,".csv"))
  write_csv(comparaison_entre_periodes_par_diplome, paste0("sorties/comparaison_entre_periodes_par_diplome_",variante,".csv"))
  write_csv(diff_diff, paste0("sorties/comparaison_entre_periodes_ecarts_entre_diplomes_",variante,".csv"))
}