# Importation des données
adultes_periodes <- readRDS("./interm/adultes_periodes.rds")
vqs <- read_delim("data/VQS/lil-1099.csv/Csv/vqsseniors.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE)


## FIT LINÉAIRE VQS
vqs <-  vqs %>%
  mutate(limite = limitat == 2 | limitat == 3,
         limite_forte = limitat == 3,
         Sexe = ifelse(sexe == 1, "Hommes", "Femmes")) %>%
  rename(AGE = age)

prevalences_vqs <- vqs %>%
  filter(AGE >= 75, AGE <= 100) %>%
  group_by(AGE, Sexe) %>%
  summarise(limite = mean(limite, na.rm = TRUE),
            limite_forte = mean(limite_forte, na.rm=TRUE),
            effectif=n())%>%
  ungroup()%>%
  pivot_longer(cols=c("limite","limite_forte"), names_to="type_limite", values_to="proportion")


# Modèle linéaire
modele_lineaire <- function(type_limite1, sexe){
  lm(data = filter(prevalences_vqs, Sexe == sexe, type_limite == type_limite1), proportion ~ AGE, weights=effectif)
}

# Calcul des R2
grille_parametres <- expand_grid(type_limite = c("limite", "limite_forte"), sexe = c("Hommes", "Femmes"))

resultats <- apply(grille_parametres, 1, function(x) { # Calcul des R²
  summary(modele_lineaire(x["type_limite"], x["sexe"]))$r.squared  # Extraire le R²
})

r2_vqs <- cbind(grille_parametres, R2 = resultats) # Créer un data.frame avec les R2


# Ajout au dataframe des interpolations linéaires
prevalences_vqs_lin <- prevalences_vqs %>%
  group_by(Sexe,type_limite)%>%
  mutate(
    proportion_lineaire = predict(modele_lineaire(type_limite, Sexe), newdata=data.frame(AGE))
  )%>%
  ungroup()%>%
  pivot_longer(cols = c("proportion","proportion_lineaire"), names_to="lissage",values_to="proportion")

# Graphe
(ggplot(data = prevalences_vqs_lin, aes(x=AGE, y = proportion, color=lissage))+
  geom_line()+
  facet_grid(Sexe ~ type_limite)+
  labs(x="Âge",y="Proportion",color="Prévalence d'incapacité \n en 2014 selon VQS")+
  scale_color_brewer(palette = "Paired", labels=c("proportion brute","lissage linéaire")))%>%
  ggsave("graphes/fit_lineaire_VQS.png",plot=.,width=largeur, height=hauteur,unit="cm")

# Fit linéaire SRCV femmes employées 2009-2013
employees_agees <- filter(adultes_periodes, Sexe == "Femmes", PCS %in% c("Employés"), periode == "2009-2013", AGE >= 75, AGE <= 100)
employees_agees_grouped <- employees_agees %>%
  group_by(AGE) %>%
  summarise(limite = weighted_mean_na_rm(limite, PB040),
            limite_forte = weighted_mean_na_rm(limite_forte, PB040),
            effectif = sum(PB040))%>%
  ungroup()
modele_lineaire_limite <- lm(data = employees_agees_grouped, limite ~ AGE, weights = effectif)
modele_lineaire_limite_forte <- lm(data = employees_agees_grouped, limite_forte ~ AGE, weights = effectif)

summary(modele_lineaire_limite)
summary(modele_lineaire_limite_forte)
employees_agees_grouped <- employees_agees_grouped %>%
  mutate(
    limite_lineaire = predict(modele_lineaire_limite, newdata=data.frame(AGE)),
    limite_forte_lineaire = predict(modele_lineaire_limite_forte, newdata=data.frame(AGE)),
  )%>%
  pivot_longer(cols = c("limite","limite_lineaire","limite_forte","limite_forte_lineaire"), names_to="variable", values_to="proportion")%>%
  mutate(
    lissage = ifelse(
      variable %in% c("limite","limite_forte"), "non lissé",
      "lissage linéaire"
    ),
    type_limite = ifelse(
      variable %in% c("limite","limite_lineaire"), "limitée",
      "fortement limitée"
    )
  ) %>%
  mutate(type_limite = factor(type_limite, levels=c("limitée","fortement limitée")))

(ggplot(employees_agees_grouped, aes(x = AGE, y = proportion, color=lissage))+
  geom_line()+
  labs(x="Âge",y="Proportion",color="Prévalence d'incapacités \n parmi les femmes employées \n en 2009-2013 selon SRCV")+
  scale_color_brewer(palette="Paired", labels=c("proportion brute","lissage linéaire"))+
  facet_wrap(~type_limite))%>%
  ggsave("graphes/fit_lineaire_employees_SRCV.png",plot=., width=largeur, height=hauteur, unit="cm")

## Zoom sur les femmes entre 70 et 79 ans ne se déclarant pas retraitées 
adultes_periodes %>%
  filter(AGE %in% c(70:79)) %>%
  group_by(periode, Sexe) %>%
  summarise(
    non_retraite = 1 - weighted.mean(retraite, PB040, na.rm=T),
    au_foyer = weighted.mean(SITUA == 6, PB040, na.rm=T)
  ) %>%
  View()