# Paramètres ####
largeur <- 18
hauteur <- largeur*0.57


for(variante in liste_variantes){
  print(variante)
  comparaison_entre_diplomes_periodes <- readRDS(paste0("interm/comparaison_entre_diplomes_",variante,".rds"))
  comparaison_entre_diplomes_periodes_tousSexes <- comparaison_entre_diplomes_periodes %>% filter(Sexe == "Ensemble")
  comparaison_entre_diplomes_periodes <- comparaison_entre_diplomes_periodes %>% filter(Sexe != "Ensemble")
  comparaison_entre_diplomes <- comparaison_entre_diplomes_periodes %>% filter(periode == "2017-2019")
  
  annee_min <- case_when(
    startsWith(variante, "SRCV") ~ 2008,
    variante == "enqEmploi" ~ 2013
  )
  
  legende <- case_when(
    variante == "SRCV" ~ "Source : SRCV",
    variante == "SRCV_lisse" ~ "Source : SRCV. Prévalences lissées.",
    variante == "SRCV_revenu" ~ "Source : SRCV. Retraite définie par le revenu.",
    variante == "SRCV_retraitebrut" ~ "Source : SRCV. Retraite non corrigée.",
    variante == "SRCV_recensement" ~ "Sources : SRCV et recensement.",
    variante == "enqEmploi" ~ "Source : enquête Emploi."
  )
  
  # EV par diplôme ####
  ev_long <- comparaison_entre_diplomes %>%
    filter(diplome %notin% c("Ensemble","Bac ou moins")) %>%
    mutate(
      ev_limite_forte = ev - evsif,
      ev_limite_moderee = evsif - evsi
    ) %>%
    select(diplome, Sexe, evsi,ev_limite_forte,ev_limite_moderee)%>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")
  
  ev_moy_long <- comparaison_entre_diplomes %>%
    filter(diplome == "Ensemble")%>%
    select(Sexe, ev, evsi, evsif) %>%
    pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")
  
  (ggplot() + 
      geom_bar(data = ev_long, aes(x=diplome,y=valeur_esperance, fill=factor(type_esperance, levels=c("ev_limite_forte","ev_limite_moderee","evsi"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme",y="Années",fill="Espérance de vie \n à 30 ans")+
      scale_fill_brewer(breaks=c("evsi","ev_limite_moderee","ev_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = ev_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed", show.legend=FALSE)+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(30, 65, by = 5), limits=c(0,60)))%>%
    ggsave(paste0("graphes/EV_par_diplome_",variante,".png"), plot=., width=largeur, height=hauteur, unit="cm")
  
  # ER par diplome ####
  er_long <- comparaison_entre_diplomes %>%
    filter(diplome %notin% c("Ensemble", "Bac ou moins"))%>%
    mutate(
      er_limite_forte = er - ersif,
      er_limite_moderee = ersif - ersi
    ) %>%
    select(diplome, Sexe, ersi,er_limite_forte,er_limite_moderee)%>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")
  
  er_moy_long <- comparaison_entre_diplomes %>%
    filter(diplome == "Ensemble")%>%
    select(Sexe, er, ersi, ersif) %>%
    pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")
  
  (ggplot() + 
      geom_bar(data = er_long, aes(x=diplome,y=valeur_esperance, fill=factor(type_esperance, levels=c("er_limite_forte","er_limite_moderee","ersi"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme",y="Années",fill="Espérance de retraite")+
      scale_fill_brewer(breaks=c("ersi","er_limite_moderee","er_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = er_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(5, 30, by = 5)))%>%
    ggsave(paste0("graphes/ER_par_diplome_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # Survie à 60 ans par diplôme ####
  survie60_long <- comparaison_entre_diplomes %>%
    filter(diplome %notin% c("Ensemble", "Bac ou moins")) %>%
    mutate(
      survie60_limite_forte = survie60 - survie60_non_limite_forte,
      survie60_limite_moderee = survie60_non_limite_forte - survie60_non_limite
    ) %>%
    select(diplome, Sexe, survie60_non_limite,survie60_limite_forte,survie60_limite_moderee)%>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")
  
  survie60_moy_long <- comparaison_entre_diplomes %>%
    filter(diplome == "Ensemble")%>%
    select(Sexe, survie60, survie60_non_limite, survie60_non_limite_forte) %>%
    pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")
  
  (ggplot() + 
      geom_bar(data = survie60_long, aes(x=diplome,y=valeur_esperance, fill=factor(type_esperance, levels=c("survie60_limite_forte","survie60_limite_moderee","survie60_non_limite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme",y="Proportion",fill="Survie à 60 ans")+
      scale_fill_brewer(breaks=c("survie60_non_limite","survie60_limite_moderee","survie60_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = survie60_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)))%>%
    ggsave(paste0("graphes/survie60_par_diplome_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # EVSI, décomposition des écarts entre diplômes ####
  diff_evsi_long <- comparaison_entre_diplomes %>%
    filter(diplome %notin% c("Ensemble", "Bac ou moins")) %>%
    select(diplome, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_evsi_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de vie \n sans incapacité, \n écart avec les diplômés \n du supérieur...")+
      scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                        labels=c("expliqué par les incapacités","expliqué par la mortalité","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_diplomes, diplome %notin% c("Ensemble", "Bac ou moins")), aes(x = diplome, y = diff_evsi), color="black", size=2)+
      geom_errorbar(data = filter(comparaison_entre_diplomes, diplome %notin% c("Ensemble", "Bac ou moins")), aes(x = diplome, ymin = diff_evsi_ci_inf, ymax=diff_evsi_ci_sup, width=0.2))+
      scale_y_continuous(breaks=c(-15:5))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_EVSI_entre_diplomes_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # ER, décomposition des écarts entre diplômes ####
  diff_er_long <- comparaison_entre_diplomes %>%
    filter(diplome %notin% c("Ensemble", "Bac ou moins")) %>%
    select(diplome, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de retraite, \n écart avec les diplômés \n du supérieur...")+
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliqué par la mortalité","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_diplomes, diplome %notin% c("Ensemble", "Bac ou moins")), aes(x = diplome, y = diff_er), color="black", size=2)+
      geom_errorbar(data = filter(comparaison_entre_diplomes, diplome %notin% c("Ensemble", "Bac ou moins")), aes(x = diplome, ymin = diff_er_ci_inf, ymax=diff_er_ci_sup, width=0.2))+
      scale_y_continuous(breaks=c(-15:5))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_diplomes_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # ERSI, décomposition des écarts entre diplômes ####
  diff_ersi_long <- comparaison_entre_diplomes %>%
    filter(diplome %notin% c("Ensemble", "Bac ou moins")) %>%
    select(diplome, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_ersi_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de retraite \n sans incapacité, \n écart avec les diplômés \n du supérieur")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliqué par la mortalité","expliqué par les incapacités","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_diplomes, diplome %notin% c("Ensemble", "Bac ou moins")), aes(x = diplome, y = diff_ersi), color="black", size=2)+
      geom_errorbar(data = filter(comparaison_entre_diplomes, diplome %notin% c("Ensemble", "Bac ou moins")), aes(x = diplome, ymin = diff_ersi_ci_inf, ymax=diff_ersi_ci_sup, width=0.2))+
      scale_y_continuous(breaks=c(-15:5))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_diplomes_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  dimensions <- c("evsi", "evsif", "er", "ersi", "ersif")
  
  # Suffixes
  suffixes <- c("", "_ci_inf", "_ci_sup")
  # Build variable names
  vars <- as.vector(outer(paste0("diff_", dimensions), suffixes, paste0))
  
  # Gradient par diplôme ####
  gradients_diplome <- comparaison_entre_diplomes_periodes %>%
    filter(diplome %notin% c("Ensemble", "Bac ou moins")) %>%
    select(Sexe, periode, diplome, diff_ev, vars) %>%
    pivot_longer(., cols=-c(Sexe, periode, diplome), names_to="dimension",values_to="contenu") %>%
    mutate(
      type_borne = case_when(
        endsWith(dimension, "ci_inf") ~ "ci_inf",
        endsWith(dimension, "ci_sup") ~ "ci_sup",
        .default = "valeur"
      ),
      dimension = sub("_ci_inf$|_ci_sup$", "", dimension)
    ) %>%
    pivot_wider(names_from = type_borne, values_from = contenu)
  
  gradients_diplome_ponctuel <- gradients_diplome %>%
    filter(periode == "2017-2019") %>%
    filter(dimension != "diff_evsif", dimension != "diff_ersif") %>%
    mutate(
      type_esperance = ifelse(dimension %in% c("diff_ev","diff_evsi","diff_evsif"), "Espérance de vie","Espérance de retraite"),
      type_esperance = factor(type_esperance, levels = c("Espérance de vie","Espérance de retraite")),
      type_limite = case_when(
        dimension %in% c("diff_ev","diff_er") ~ "totale",
        dimension %in% c("diff_evsi","diff_ersi") ~ "sans incapacité"
      ),
      type_limite = factor(type_limite, levels = c("totale","sans incapacité"))
    ) %>%
    select(-dimension)
  
  (ggplot(data = gradients_diplome_ponctuel, aes(x = diplome, y=valeur, color=type_limite, group=type_limite))+
      geom_point(size=2)+
      geom_line()+
      geom_errorbar(aes(ymin = ci_inf, ymax=ci_sup), width=0.2)+
      facet_grid(type_esperance~Sexe)+
      labs(caption = legende, x =  "Diplôme",y ="Années", color="")+
      scale_color_brewer(palette="Paired")+
      scale_y_continuous(breaks=seq(-10,4,by=2))) %>%
    ggsave(paste0("graphes/gradients_diplome_",variante,".png"), plot=., width=largeur, height=hauteur, unit="cm")
  
  # Tous sexes confondus
  gradients_diplome_tousSexes <- comparaison_entre_diplomes_periodes_tousSexes %>%
    filter(diplome %notin% c("Ensemble", "Bac ou moins")) %>%
    select(periode, diplome, diff_ev, vars) %>%
    pivot_longer(., cols=-c(periode, diplome), names_to="dimension",values_to="contenu") %>%
    mutate(
      type_borne = case_when(
        endsWith(dimension, "ci_inf") ~ "ci_inf",
        endsWith(dimension, "ci_sup") ~ "ci_sup",
        .default = "valeur"
      ),
      dimension = sub("_ci_inf$|_ci_sup$", "", dimension)
    ) %>%
    pivot_wider(names_from = type_borne, values_from = contenu)
  
  gradients_diplome_ponctuel_tousSexes <- gradients_diplome_tousSexes %>%
    filter(periode == "2017-2019") %>%
    filter(dimension != "diff_evsif", dimension != "diff_ersif") %>%
    mutate(
      type_esperance = ifelse(dimension %in% c("diff_ev","diff_evsi","diff_evsif"), "Espérance de vie","Espérance de retraite"),
      type_esperance = factor(type_esperance, levels = c("Espérance de vie","Espérance de retraite")),
      type_limite = case_when(
        dimension %in% c("diff_ev","diff_er") ~ "totale",
        dimension %in% c("diff_evsi","diff_ersi") ~ "sans incapacité"
      ),
      type_limite = factor(type_limite, levels = c("totale","sans incapacité"))
    ) %>%
    select(-dimension)
  
  (ggplot(data = gradients_diplome_ponctuel_tousSexes, aes(x = diplome, y=valeur, color=type_limite, group=type_limite))+
      geom_point(size=2)+
      geom_line()+
      geom_errorbar(aes(ymin = ci_inf, ymax=ci_sup), width=0.2)+
      facet_wrap(~type_esperance)+
      labs(caption = paste0(legende,". Tous sexes confondus."), x =  "Diplôme",y ="Années", color="")+
      scale_color_brewer(palette="Paired")+
      scale_y_continuous(breaks=seq(-10,4,by=2))) %>%
    ggsave(paste0("graphes/gradients_diplome_",variante,"_tousSexes.png"), plot=., width=largeur, height=hauteur, unit="cm")
  
  
  # Gradients diplômes ER, limite simple seulement ####
  (ggplot(data = filter(gradients_diplome_ponctuel, type_esperance == "Espérance de retraite", type_limite %in% c("totale","sans incapacité")),
          aes(x = diplome, y=valeur, color=type_limite, group=type_limite))+
     geom_point(size=2)+
     geom_line()+
     geom_errorbar(aes(ymin = ci_inf, ymax=ci_sup), width=0.2)+
     facet_wrap(~Sexe)+
     labs(caption = legende, x =  "Diplôme",y ="Années", color="Écart avec les diplômés \n du supérieur en matière \n d'espérance de retraite...")+
     scale_color_brewer(palette="Paired")+
     scale_y_continuous(breaks=seq(-8,4,by=2))) %>%
    ggsave(paste0("graphes/gradients_diplome_ER_",variante,".png"), plot=., width=largeur, height=hauteur, unit="cm")
  

  if(endsWith(variante,"recensement")){
    next
    }
  
  comparaison_entre_sexes <- readRDS(paste0("interm/comparaison_entre_sexes_",variante,".rds")) %>% filter(Sexe != "Ensemble", periode == "2017-2019") 
  comparaison_entre_PCS_periodes <- readRDS(paste0("interm/comparaison_entre_PCS_",variante,".rds"))
  comparaison_entre_PCS_periodes_tousSexes <- comparaison_entre_PCS_periodes %>% filter(Sexe == "Ensemble")
  comparaison_entre_PCS_periodes <- comparaison_entre_PCS_periodes %>% filter(Sexe != "Ensemble")
  comparaison_entre_PCS <- comparaison_entre_PCS_periodes %>% filter(periode == "2017-2019") 
  comparaison_entre_periodes_par_PCS <- readRDS(paste0("interm/comparaison_entre_periodes_par_PCS_",variante,".rds")) %>% filter(Sexe != "Ensemble", periode == "2017-2019")
  comparaison_entre_periodes_par_diplome <- readRDS(paste0("interm/comparaison_entre_periodes_par_diplome_",variante,".rds")) %>% filter(Sexe != "Ensemble", periode == "2017-2019")
  comparaison_entre_periodes_ecarts_entre_diplomes <- readRDS(paste0("interm/comparaison_entre_periodes_ecarts_entre_diplomes_",variante,".rds")) %>% filter(Sexe != "Ensemble")
  
  # EV par PCS ####
  ev_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble") %>%
    mutate(
      ev_limite_forte = ev - evsif,
      ev_limite_moderee = evsif - evsi
    ) %>%
    select(PCS, Sexe, evsi,ev_limite_forte,ev_limite_moderee)%>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")
  
  ev_moy_long <- comparaison_entre_PCS %>%
    filter(PCS == "Ensemble")%>%
    select(Sexe, ev, evsi, evsif) %>%
    pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")
  
  (ggplot() + 
    geom_bar(data = ev_long, aes(x=PCS,y=valeur_esperance, fill=factor(type_esperance, levels=c("ev_limite_forte","ev_limite_moderee","evsi"))),
             position="stack", stat="identity")+
    labs(caption = legende, x = "PCS",y="Années",fill="Espérance de vie \n à 30 ans")+
    scale_fill_brewer(breaks=c("evsi","ev_limite_moderee","ev_limite_forte"),
                      labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                      palette="YlOrBr", direction=-1)+
    geom_hline(data = ev_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed", show.legend=FALSE)+
    facet_wrap(~Sexe)+
    scale_y_continuous(breaks = seq(30, 65, by = 5), limits=c(0,60)))%>%
    ggsave(paste0("graphes/EV_par_PCS_",variante,".png"), plot=., width=largeur, height=hauteur, unit="cm")
    
  # ER par PCS ####
  er_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble")%>%
    mutate(
      er_limite_forte = er - ersif,
      er_limite_moderee = ersif - ersi
    ) %>%
    select(PCS, Sexe, ersi,er_limite_forte,er_limite_moderee)%>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")
  
  er_moy_long <- comparaison_entre_PCS %>%
    filter(PCS == "Ensemble")%>%
    select(Sexe, er, ersi, ersif) %>%
    pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")
  
  (ggplot() + 
      geom_bar(data = er_long, aes(x=PCS,y=valeur_esperance, fill=factor(type_esperance, levels=c("er_limite_forte","er_limite_moderee","ersi"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "PCS",y="Années",fill="Espérance de retraite")+
      scale_fill_brewer(breaks=c("ersi","er_limite_moderee","er_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = er_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(5, 30, by = 5)))%>%
    ggsave(paste0("graphes/ER_par_PCS_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # Survie à 60 ans par PCS ####
  survie60_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble") %>%
    mutate(
      survie60_limite_forte = survie60 - survie60_non_limite_forte,
      survie60_limite_moderee = survie60_non_limite_forte - survie60_non_limite
    ) %>%
    select(PCS, Sexe, survie60_non_limite,survie60_limite_forte,survie60_limite_moderee)%>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")
  
  survie60_moy_long <- comparaison_entre_PCS %>%
    filter(PCS == "Ensemble")%>%
    select(Sexe, survie60, survie60_non_limite, survie60_non_limite_forte) %>%
    pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")
  
  (ggplot() + 
      geom_bar(data = survie60_long, aes(x=PCS,y=valeur_esperance, fill=factor(type_esperance, levels=c("survie60_limite_forte","survie60_limite_moderee","survie60_non_limite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "PCS",y="Proportion",fill="Survie à 60 ans")+
      scale_fill_brewer(breaks=c("survie60_non_limite","survie60_limite_moderee","survie60_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = survie60_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)))%>%
    ggsave(paste0("graphes/survie60_par_PCS_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # EVSI, décomposition des écarts entre PCS ####
  diff_evsi_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble") %>%
    select(PCS, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu, diff_evsi_ci_inf, diff_evsi_ci_sup) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_evsi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "PCS", y="Années",fill="Espérance de vie \n sans incapacité, \n écart avec les cadres")+
      scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                        labels=c("expliqué par les incapacités","expliqué par la mortalité","Résidu"),
                        palette="YlOrBr", direction=-1)+
      scale_y_continuous(breaks=c(-15:5))+
      geom_point(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, y = diff_evsi), color="black", size=2)+
      geom_errorbar(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, ymin = diff_evsi_ci_inf, ymax=diff_evsi_ci_sup, width=0.2))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_EVSI_entre_PCS_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # ER, décomposition des écarts entre PCS ####
  diff_er_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble") %>%
    select(PCS, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "PCS", y="Années",fill="Espérance de retraite, \n écart avec les cadres...")+
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliqué par la mortalité","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      scale_y_continuous(breaks=c(-15:5))+
      geom_point(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, y = diff_er), color="black", size=2)+
      geom_errorbar(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, ymin = diff_er_ci_inf, ymax=diff_er_ci_sup, width=0.2))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_PCS_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # ERSI, décomposition des écarts entre PCS ####
  diff_ersi_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble") %>%
    select(PCS, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_ersi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "PCS", y="Années",fill="Espérance de retraite \n sans incapacité, \n écart avec les cadres")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliqué par la mortalité","expliqué par les incapacités","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, y = diff_ersi), color="black", size=2)+
      geom_errorbar(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, ymin = diff_ersi_ci_inf, ymax=diff_ersi_ci_sup, width=0.2))+
      scale_y_continuous(breaks=c(-15:5))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_PCS_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # Gradient par PCS ####
  gradients_PCS <- comparaison_entre_PCS_periodes %>%
    filter(PCS != "Ensemble") %>%
    select("Sexe", "periode", "PCS", "diff_ev", vars) %>%
    pivot_longer(., cols=-c(Sexe, periode, PCS), names_to="dimension",values_to="contenu") %>%
    mutate(
      type_borne = case_when(
        endsWith(dimension, "ci_inf") ~ "ci_inf",
        endsWith(dimension, "ci_sup") ~ "ci_sup",
        .default = "valeur"
      ),
      dimension = sub("_ci_inf$|_ci_sup$", "", dimension)
    ) %>%
    pivot_wider(names_from = type_borne, values_from = contenu)
  
  gradients_PCS_ponctuel <- gradients_PCS %>%
    filter(periode == "2017-2019", dimension %notin% c("diff_evsif", "diff_ersif")) %>%
    mutate(
      type_esperance = ifelse(dimension %in% c("diff_ev","diff_evsi","diff_evsif"), "Espérance de vie","Espérance de retraite"),
      type_esperance = factor(type_esperance, levels = c("Espérance de vie","Espérance de retraite")),
      type_limite = case_when(
        dimension %in% c("diff_ev","diff_er") ~ "totale",
        dimension %in% c("diff_evsi","diff_ersi") ~ "sans incapacité"
      ),
      type_limite = factor(type_limite, levels = c("totale","sans incapacité"))
    ) %>%
    select(-dimension)
  
  (ggplot(data = gradients_PCS_ponctuel, aes(x = PCS, y=valeur, color=type_limite, group=type_limite))+
      geom_point(size=2)+
      geom_line()+
      geom_errorbar(aes(ymin = ci_inf, ymax=ci_sup), width=0.2)+
      facet_grid(type_esperance~Sexe)+
      labs(caption = legende, x =  "PCS",y ="Années", color="")+
      scale_color_brewer(palette="Paired")+
      scale_y_continuous(breaks=seq(-10,4,by=2))) %>%
    ggsave(paste0("graphes/gradients_PCS_",variante,".png"), plot=., width=largeur, height=hauteur, unit="cm")
  
  # Tous sexes confondus
    gradients_PCS_tousSexes <- comparaison_entre_PCS_periodes_tousSexes %>%
    filter(PCS != "Ensemble") %>%
    select("Sexe", "periode", "PCS", "diff_ev", vars) %>%
    pivot_longer(., cols=-c(Sexe, periode, PCS), names_to="dimension",values_to="contenu") %>%
    mutate(
      type_borne = case_when(
        endsWith(dimension, "ci_inf") ~ "ci_inf",
        endsWith(dimension, "ci_sup") ~ "ci_sup",
        .default = "valeur"
      ),
      dimension = sub("_ci_inf$|_ci_sup$", "", dimension)
    ) %>%
    pivot_wider(names_from = type_borne, values_from = contenu)
  
  gradients_PCS_ponctuel_tousSexes <- gradients_PCS_tousSexes %>%
    filter(periode == "2017-2019", dimension %notin% c("diff_evsif", "diff_ersif")) %>%
    mutate(
      type_esperance = ifelse(dimension %in% c("diff_ev","diff_evsi","diff_evsif"), "Espérance de vie","Espérance de retraite"),
      type_esperance = factor(type_esperance, levels = c("Espérance de vie","Espérance de retraite")),
      type_limite = case_when(
        dimension %in% c("diff_ev","diff_er") ~ "totale",
        dimension %in% c("diff_evsi","diff_ersi") ~ "sans incapacité"
      ),
      type_limite = factor(type_limite, levels = c("totale","sans incapacité"))
    ) %>%
    select(-dimension)
  
  (ggplot(data = gradients_PCS_ponctuel_tousSexes, aes(x = PCS, y=valeur, color=type_limite, group=type_limite))+
      geom_point(size=2)+
      geom_line()+
      geom_errorbar(aes(ymin = ci_inf, ymax=ci_sup), width=0.2)+
      facet_wrap(~type_esperance)+
      labs(caption = paste0(legende," Tous sexes confondus."), x =  "PCS",y ="Années", color="")+
      scale_color_brewer(palette="Paired")+
      scale_y_continuous(breaks=seq(-10,4,by=2))) %>%
    ggsave(paste0("graphes/gradients_PCS_",variante,"_tousSexes.png"), plot=., width=largeur, height=hauteur, unit="cm")

  
  # Écarts entre sexes, décomposition ####
  ev_hommes <- comparaison_entre_sexes %>% filter(Sexe == "Hommes") %>% select(ev) %>% pull()
  
  diff_sexe_long <- comparaison_entre_sexes %>%
    select(
      -any_of(
        names(comparaison_entre_sexes)[endsWith(names(comparaison_entre_sexes), "_ci_inf") | endsWith(names(comparaison_entre_sexes), "_ci_sup") | endsWith(names(comparaison_entre_sexes), "_var")]
      )
    ) %>%
    filter(Sexe == "Femmes") %>%
    mutate(diff_ev = ev - ev_hommes) %>% mutate(diff_ev_survie = diff_ev) %>%
    select(starts_with("diff_"), -starts_with("diff_evsif"), -starts_with("diff_ersif"))%>%
    pivot_longer(.,everything(), names_to = "variable", values_to = "valeur") %>%
    mutate(
      type_esperance = sapply(strsplit(variable, "_"), "[", 2),
      composant = sapply(strsplit(variable, "_"), "[", 3)
    )
  
  (ggplot()+
      geom_bar(data = filter(diff_sexe_long, !is.na(composant)), aes(x = factor(type_esperance, levels=c("ev","er","evsi","ersi")), y=valeur, fill=factor(composant, levels=c("residu", "survie","sante","retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "", y="Années",fill="Écart des femmes \n avec les hommes...")+
      scale_x_discrete(labels=c("Espérance de vie","Espérance de retraite","Espérance de vie \n sans incapacité","Espérance de retraite \n sans incapacité"))+
      scale_y_continuous(breaks = seq(-2,6, by=1))+
      scale_fill_brewer(breaks=c("survie","sante","retraite","residu"),
                        labels=c("expliqué par la mortalité","expliqué par les incapacités","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
      geom_point(data = filter(diff_sexe_long, is.na(composant)), aes(x = type_esperance, y = valeur), color="black", size=2)
  ) %>%
    ggsave(paste0("graphes/diff_entre_sexes_",variante,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  
  if(variante == "enqEmploi"){
    next
  }
  
  # Gradient PCS, évolution entre périodes ####
  gradients_PCS_periodes <- gradients_PCS %>%
    filter(dimension %notin% c("diff_evsif","diff_ersif")) %>%
    mutate(
      dimension = factor(dimension,
                         levels=c("diff_ev","diff_evsi","diff_er","diff_ersi"),
                         labels=c("Espérance de vie", "Espérance de vie \n sans incapacité","Espérance de retraite","Espérance de retraite \n sans incapacité"))
    )
  
  (ggplot(data = gradients_PCS_periodes, aes(x=PCS, y=valeur, color=periode, group=periode))+
    geom_point(size=2)+
    geom_line()+
    geom_errorbar(aes(ymin = ci_inf, ymax=ci_sup), width=0.2)+
    labs(caption = legende, x = "PCS", y = "Années", color="Période")+
    scale_color_brewer(palette = "Paired")+
    scale_y_continuous(breaks=c(-10:0))+
    facet_grid(Sexe ~ dimension)+
    theme(legend.position="bottom")
    )%>%
    ggsave(paste0("graphes/gradients_PCS_periodes_",variante,".png"),plot=., width=largeur*1.5, height=hauteur*1.5, unit="cm")
  
  # Évolution des écarts entre périodes ####
  diff_diff2 <- comparaison_entre_periodes_ecarts_entre_diplomes %>%
    filter(diplome == "Bac ou moins")
  diff_diff_ersi_long <- diff_diff2 %>%
    select(Sexe,diff_ersi_survie,diff_ersi_sante,diff_ersi_retraite,diff_ersi_residu) %>%
    pivot_longer(., cols=-Sexe, names_to = "composant", values_to = "valeur")
  
  (ggplot()+
    geom_bar(data = diff_diff_ersi_long, aes(x = Sexe, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
             position="stack", stat="identity")+
    labs(caption = legende, x = "Sexe", y="Années",fill="ERSI, évolution entre périodes \n de l'écart entre les \n titulaires du bac ou moins \n avec les diplômés du supérieur")+
    scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                      labels=c("expliquée par la mortalité","expliquée par les incapacités","expliquée par la retraite","Résidu"),
                      palette="YlOrBr", direction=-1)+
    geom_point(data = diff_diff2, aes(x = Sexe, y = diff_ersi), color="black", size=2)+
    geom_errorbar(data = diff_diff2, aes(x = Sexe, ymin = diff_ersi_ci_inf, ymax=diff_ersi_ci_sup), color="black", width=0.2))%>%
    ggsave(paste0("graphes/diff_diff_ERSI_diplome_",variante,".png"), plot=., width= largeur, height=hauteur, unit="cm")
  
  diff_diff_er_long <- diff_diff2 %>%
    select(Sexe,diff_er_survie,diff_er_retraite,diff_er_residu) %>%
    pivot_longer(., cols=-Sexe, names_to = "composant", values_to = "valeur")
  
  (ggplot()+
    geom_bar(data = diff_diff_er_long, aes(x = Sexe, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
             position="stack", stat="identity")+
    labs(caption = legende, x = "Sexe", y="Années",fill="ER, évolution entre périodes \n de l'écart entre les \n titulaires du bac ou moins \n avec les diplômés du supérieur")+
    scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                      labels=c("expliquée par la mortalité","expliquée par la retraite","Résidu"),
                      palette="YlOrBr", direction=-1)+
    geom_point(data = diff_diff2, aes(x = Sexe, y = diff_er), color="black", size=2)+
    geom_errorbar(data = diff_diff2, aes(x = Sexe, ymin = diff_er_ci_inf, ymax=diff_er_ci_sup), color="black", width=0.2))%>%
    ggsave(paste0("graphes/diff_diff_ER_diplome_",variante,".png"), plot=., width= largeur, height=hauteur, unit="cm")
  
  # Gradients diplôme, évolution entre périodes ####
  gradients_diplome_periodes <- gradients_diplome %>%
    filter(dimension %notin% c("diff_evsif","diff_ersif")) %>%
    mutate(
      dimension = factor(dimension,
                         levels=c("diff_ev","diff_evsi","diff_er","diff_ersi"),
                         labels=c("Espérance de vie", "Espérance de vie \n sans incapacité","Espérance de retraite","Espérance de retraite \n sans incapacité"))
    )
  
  (ggplot(data = gradients_diplome_periodes, aes(x=diplome, y=valeur, color=periode, group=periode))+
      geom_point(size=2)+
      geom_line()+
      geom_errorbar(aes(ymin = ci_inf, ymax=ci_sup), width=0.2)+
      labs(caption = legende, x = "Diplôme", y = "Années", color="Période")+
      scale_color_brewer(palette = "Paired")+
      scale_y_continuous(breaks=c(-10:0))+
      facet_grid(Sexe ~ dimension)+
      theme(legend.position="bottom"))%>%
    ggsave(paste0("graphes/gradients_diplome_periodes_",variante,".png"),plot=., width=largeur*1.5, height=hauteur*1.5, unit="cm")
  
  
  
  # Évolution de l'EVSI entre péridoes par PCS ###
  diff_evsi_long <- comparaison_entre_periodes_par_PCS %>%
    select(PCS, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_evsi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "PCS", y="Années",fill="Espérance de vie \n sans incapacité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                        labels=c("expliquée par les incapacités","expliquée par la mortalité","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, y = diff_evsi), color="black", size=2)+
      geom_errorbar(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, ymin = diff_evsi_ci_inf, ymax = diff_evsi_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_EVSI_entre_periodes_par_PCS_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  # Évolution de l'ER entre périodes par PCS ###
  diff_er_long <- comparaison_entre_periodes_par_PCS %>%
    select(PCS, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "PCS", y="Années",fill="Espérance de retraite, \n évolution entre 2009-2013 \n et 2017-2019")+
      
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliquée par la mortalité","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, y = diff_er), color="black", size=2)+
      geom_errorbar(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, ymin = diff_er_ci_inf, ymax = diff_er_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_periodes_par_PCS_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  # Évolution de l'ERSI entre périodes par PCS ####
  diff_ersi_long <- comparaison_entre_periodes_par_PCS %>%
    select(PCS, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_ersi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "PCS", y="Années",fill="Espérance de retraite \n sans incapacité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliquée par la mortalité","expliquée par les incapacités","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, y = diff_ersi), color="black", size=2)+
      geom_errorbar(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, ymin = diff_ersi_ci_inf, ymax = diff_ersi_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_periodes_par_PCS_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  # Évolution de l'EVSI entre périodes par diplôme ####
  diff_evsi_long <- comparaison_entre_periodes_par_diplome %>%
    select(diplome, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_evsi_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de vie \n sans incapacité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                        labels=c("expliquée par les incapacités","expliquée par la mortalité","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, y = diff_evsi), color="black", size=2)+
      geom_errorbar(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, ymin = diff_evsi_ci_inf, ymax = diff_evsi_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_EVSI_entre_periodes_par_diplome_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  # Évolution de l'ER entre périodes par diplôme ####
  diff_er_long <- comparaison_entre_periodes_par_diplome %>%
    select(diplome, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de retraite, \n évolution entre 2009-2013 \n et 2017-2019")+
      
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliquée par la mortalité","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, y = diff_er), color="black", size=2)+
      geom_errorbar(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, ymin = diff_er_ci_inf, ymax = diff_er_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_periodes_par_diplome_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de retraite, \n évolution entre 2009-2013 \n et 2017-2019")+
      
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliquée par la mortalité","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, y = diff_er), color="black", size=2)+
      geom_errorbar(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, ymin = diff_er_ci_inf, ymax = diff_er_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_periodes_par_diplome_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")

  (ggplot()+
      geom_bar(data = filter(diff_er_long, diplome %in% c("Supérieur","Bac ou moins","Ensemble")),
              aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de retraite, \n évolution entre 2009-2013 \n et 2017-2019")+
      
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliquée par la mortalité","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_periodes_par_diplome, diplome %in% c("Supérieur","Bac ou moins","Ensemble")), aes(x = diplome, y = diff_er), color="black", size=2)+
      geom_errorbar(data = filter(comparaison_entre_periodes_par_diplome, diplome %in% c("Supérieur","Bac ou moins","Ensemble")), aes(x = diplome, ymin = diff_er_ci_inf, ymax = diff_er_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_periodes_par_diplome_agrege_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  
  # Évolution de l'ERSI entre périodes par diplôme ####
  diff_ersi_long <- comparaison_entre_periodes_par_diplome %>%
    select(diplome, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_ersi_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de retraite \n sans incapacité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliquée par la mortalité","expliquée par les incapacités","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, y = diff_ersi), color="black", size=2)+
      geom_errorbar(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, ymin = diff_ersi_ci_inf, ymax=diff_ersi_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_periodes_par_diplome_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  (ggplot()+
      geom_bar(data = filter(diff_ersi_long, diplome %in% c("Supérieur","Bac ou moins","Ensemble")),
               aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = legende, x = "Diplôme", y="Années",fill="Espérance de retraite \n sans incapacité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliquée par la mortalité","expliquée par les incapacités","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_periodes_par_diplome, diplome %in% c("Supérieur","Bac ou moins","Ensemble")), aes(x = diplome, y = diff_ersi), color="black", size=2)+
      geom_errorbar(data = filter(comparaison_entre_periodes_par_diplome, diplome %in% c("Supérieur","Bac ou moins","Ensemble")), aes(x = diplome, ymin = diff_ersi_ci_inf, ymax=diff_ersi_ci_sup), color="black", width=0.2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_periodes_par_diplome_agrege_",variante,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  
  # Évolution des écarts par diplôme d'ERSI entre périodes
}