# Renormaliser la survie pour qu'elle soit de 100% à 30 ans
renormaliser_survie <- function(df, ...){
  df %>%
    mutate(survie_brut = survie)%>%
    group_by(...) %>%
    mutate(survie = survie_brut / survie_brut[AGE == 30]) %>%
    select(-survie_brut)
}


# Fonction pour charger un fichier de mortalité
charger_mortalite <- function(critere, sexe, periode_arg){
  read_delim(paste0("./data/Mortalité/Mortalite-", case_when(critere == "PCS" ~ "CS", critere == "diplome" ~ "diplome"),
                    "-FE-", substr(sexe,1,1),
                    "-",periode_arg,".csv"),
             delim=";") %>%
    pivot_longer(cols = -age, names_to = critere, values_to = "survie") %>%
    rename(AGE = age) %>%
    mutate(periode = periode_arg,
           Sexe = sexe,
           survie = survie/100000) %>%
    arrange(periode,!!sym(critere),Sexe,AGE)%>%
    renormaliser_survie(!!sym(critere))
}


# Compléter les colonnes en combinant les informations disponibles
completer_colonnes <- function(df){
  df %>%
    mutate(
      survie_non_limite = survie*non_limite,
      survie_retraite = survie*retraite,
      survie_retraite_non_limite = survie*retraite_non_limite,
      survie_non_limite_forte = survie*non_limite_forte,
      survie_retraite_non_limite_forte = survie*retraite_non_limite_forte
    )
}

# On construit les dataframes de survie par PCS et periode
sexes <- c("Femmes", "Hommes")
periodes <- c("2009-2013", "2017-2019")

survie_PCS <- expand_grid(sexe = sexes, periode = periodes) %>%
  pmap_dfr(~ charger_mortalite("PCS", ..1, ..2))

survie_diplome <- expand_grid(sexe = sexes, periode = periodes) %>%
  pmap_dfr(~ charger_mortalite("diplome", ..1, ..2)) %>%
  mutate(
    diplome = case_when(
      diplome == "Superieur" ~ "Supérieur",
      diplome == "SansDiplome" ~ "Sans",
      .default = diplome
    )
  )



# On charge les données de mortalité par genre en série longue
# Les données INSEE de mortalité par génération viennent d'ici : https://www.insee.fr/fr/statistiques/6543678?sommaire=6543680
# Elles ont été préalablement converties manuellement en .csv
survie_SL <- map_dfr(sexes,
                     ~read_delim(paste0("./data/Mortalité/MortaliteGeneration",substr(.x,1,1),".csv"), 
                                delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),trim_ws = TRUE)%>%
                       pivot_longer(col=-1, values_to = "mortalite", names_to = "AGE")%>%
                       mutate(Sexe = .x)
                     )

survie_SL <- survie_SL %>%
              mutate(AGE = as.numeric(AGE),
                     AENQ = birth + AGE) %>% # annee = naissance + age
              filter(AGE >= 30, AGE <= 100, AENQ >= 2008, AENQ <= 2019) %>%
              arrange(Sexe, AENQ, AGE) %>% 
              group_by(AENQ, Sexe) %>%
              mutate(survie = cumprod(1 - lag(mortalite,default=0)/100000)) %>% # On calcule la survie instantanée
              ungroup()%>%
              select(-birth, -mortalite)
              # Pas besoin de renormalisation ici puisqu'on a construit la variable survie nous-mêmes en partant des mortalités à partir de 30 ans

# Fusion avec les prévalences
# On charge les données de limitations*retraite
for(variante in c("SRCV","SRCV_agecat","SRCV_revenu","enqEmploi")){
  prevalences_PCS <- readRDS(paste0("./interm/prevalences_PCS_",variante,".rds"))
  prevalences_diplome <- readRDS(paste0("./interm/prevalences_diplome_",variante,".rds"))
  prevalences_SL <- readRDS(paste0("./interm/prevalences_SL_",variante,".rds"))
  
  prevalences_survie_PCS <- merge(prevalences_PCS,survie_PCS,by=c("periode","PCS","Sexe","AGE"))%>%
    completer_colonnes%>%
    arrange(periode, PCS, Sexe, AGE)
  
  prevalences_survie_diplome <- merge(prevalences_diplome,survie_diplome,by=c("periode","diplome","Sexe","AGE"))%>%
    completer_colonnes%>%
    arrange(periode, diplome, Sexe, AGE)
  
  prevalences_survie_SL <- merge(prevalences_SL,survie_SL,by=c("AENQ","Sexe","AGE"))%>%
    completer_colonnes %>%
    arrange(AENQ, Sexe, AGE)
  
  saveRDS(prevalences_survie_PCS,paste0("interm/prevalences_survie_PCS_",variante,".rds")) # On sauve séparément en RDS pour garder le format factor
  saveRDS(prevalences_survie_diplome,paste0("interm/prevalences_survie_diplome_",variante,".rds"))
  saveRDS(prevalences_survie_SL,paste0("interm/prevalences_survie_SL_",variante,".rds"))
  
  write_csv(prevalences_survie_PCS,paste0("sorties/prevalences_survie_PCS_",variante,".csv"))
  write_csv(prevalences_survie_diplome,paste0("sorties/prevalences_survie_diplome_",variante,".csv"))
  write_csv(prevalences_survie_SL,paste0("sorties/prevalences_survie_SL",variante,".csv"))
}