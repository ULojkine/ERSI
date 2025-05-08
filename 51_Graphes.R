# PARAMÈTRES
largeur <- 18
hauteur <- largeur*0.57

# IMPORTATION
for(enquete in c("SRCV","enqEmploi")){
  comparaison_entre_sexes <- readRDS(paste0("interm/comparaison_entre_sexes_",enquete,".rds")) %>% filter(periode == "2017-2019") 
  comparaison_entre_PCS_periodes <- readRDS(paste0("interm/comparaison_entre_PCS_",enquete,".rds"))
  comparaison_entre_PCS <- comparaison_entre_PCS_periodes %>% filter(periode == "2017-2019") 
  comparaison_entre_diplomes_periodes <- readRDS(paste0("interm/comparaison_entre_diplomes_",enquete,".rds"))
  comparaison_entre_diplomes <- comparaison_entre_diplomes_periodes %>% filter(periode == "2017-2019")
  comparaison_entre_periodes_par_PCS <- readRDS(paste0("interm/comparaison_entre_periodes_par_PCS_",enquete,".rds")) %>% filter(periode == "2017-2019")
  comparaison_entre_periodes_par_diplome <- readRDS(paste0("interm/comparaison_entre_periodes_par_diplome_",enquete,".rds")) %>% filter(periode == "2017-2019")
  comparaison_entre_annees <- readRDS(paste0("interm/comparaison_entre_annees_",enquete,".rds"))
  
  nom_enquete <- case_when(
    enquete == "SRCV" ~ "SRCV",
    enquete == "enqEmploi" ~ "Enquête Emploi"
  )
  
  annee_min <- case_when(
    enquete == "SRCV" ~ 2008,
    enquete == "enqEmploi" ~ 2013
  )
  
  # ESPERANCE DE VIE PAR PCS
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
    labs(caption = paste0("Source : ",nom_enquete), x = "PCS",y="Années",fill="Espérance de vie \n à 30 ans")+
    scale_fill_brewer(breaks=c("evsi","ev_limite_moderee","ev_limite_forte"),
                      labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                      palette="YlOrBr", direction=-1)+
    geom_hline(data = ev_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed", show.legend=FALSE)+
    facet_wrap(~Sexe)+
    scale_y_continuous(breaks = seq(30, 65, by = 5), limits=c(0,60)))%>%
    ggsave(paste0("graphes/EV_par_PCS_",enquete,".png"), plot=., width=largeur, height=hauteur, unit="cm")
    
  ## ESPERANCE DE RETRAITE PAR PCS
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
      labs(caption = paste0("Source : ",nom_enquete), x = "PCS",y="Années",fill="Espérance de retraite")+
      scale_fill_brewer(breaks=c("ersi","er_limite_moderee","er_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = er_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(5, 30, by = 5)))%>%
    ggsave(paste0("graphes/ER_par_PCS_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## PROBABILITES DE SURVIE A 60 ANS, PAR PCS
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
      labs(caption = paste0("Source : ",nom_enquete), x = "PCS",y="Proportion",fill="Survie à 60 ans")+
      scale_fill_brewer(breaks=c("survie60_non_limite","survie60_limite_moderee","survie60_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = survie60_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)))%>%
    ggsave(paste0("graphes/survie60_par_PCS_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## EVSI, DECOMPOSITION DES ECARTS ENTRE PCS
  diff_evsi_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble") %>%
    select(PCS, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_evsi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "PCS", y="Années",fill="Espérance de vie \n sans incapacité, \n écart avec les cadres")+
      scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                        labels=c("expliqué par la santé","expliqué par la mortalité","Résidu"),
                        palette="YlOrBr", direction=-1)+
      scale_y_continuous(breaks=c(-15:5))+
      geom_point(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, y = diff_evsi), color="black", size=2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_EVSI_entre_PCS_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ESPERANCE DE RETRAITE, DECOMPOSITION DES ECARTS ENTRE PCS
  diff_er_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble") %>%
    select(PCS, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "PCS", y="Années",fill="Espérance de retraite, \n écart avec les cadres...")+
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliqué par la mortalité","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      scale_y_continuous(breaks=c(-15:5))+
      geom_point(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, y = diff_er), color="black", size=2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_PCS_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ERSI, DECOMPOSITION DES ECARTS ENTRE PCS
  diff_ersi_long <- comparaison_entre_PCS %>%
    filter(PCS != "Ensemble") %>%
    select(PCS, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_ersi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "PCS", y="Années",fill="Espérance de retraite \n sans invalidité, \n écart avec les cadres")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliqué par la mortalité","expliqué par la santé","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_PCS, PCS != "Ensemble"), aes(x = PCS, y = diff_ersi), color="black", size=2)+
      scale_y_continuous(breaks=c(-15:5))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_PCS_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  # ESPERANCE DE VIE PAR diplome
  ev_long <- comparaison_entre_diplomes %>%
    filter(diplome != "Ensemble") %>%
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
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme",y="Années",fill="Espérance de vie \n à 30 ans")+
      scale_fill_brewer(breaks=c("evsi","ev_limite_moderee","ev_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = ev_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed", show.legend=FALSE)+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(30, 65, by = 5), limits=c(0,60)))%>%
    ggsave(paste0("graphes/EV_par_diplome_",enquete,".png"), plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ESPERANCE DE RETRAITE PAR diplome
  er_long <- comparaison_entre_diplomes %>%
    filter(diplome != "Ensemble")%>%
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
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme",y="Années",fill="Espérance de retraite")+
      scale_fill_brewer(breaks=c("ersi","er_limite_moderee","er_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = er_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(5, 30, by = 5)))%>%
    ggsave(paste0("graphes/ER_par_diplome_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## PROBABILITES DE SURVIE A 60 ANS, PAR diplome
  survie60_long <- comparaison_entre_diplomes %>%
    filter(diplome != "Ensemble") %>%
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
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme",y="Proportion",fill="Survie à 60 ans")+
      scale_fill_brewer(breaks=c("survie60_non_limite","survie60_limite_moderee","survie60_limite_forte"),
                        labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                        palette="YlOrBr", direction=-1)+
      geom_hline(data = survie60_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
      facet_wrap(~Sexe)+
      scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)))%>%
    ggsave(paste0("graphes/survie60_par_diplome_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## EVSI, DECOMPOSITION DES ECARTS ENTRE diplome
  diff_evsi_long <- comparaison_entre_diplomes %>%
    filter(diplome != "Ensemble") %>%
    select(diplome, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_evsi_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme", y="Années",fill="Espérance de vie \n sans incapacité, \n écart avec les diplômés \n du supérieur...")+
      scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                        labels=c("expliqué par la santé","expliqué par la mortalité","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_diplomes, diplome != "Ensemble"), aes(x = diplome, y = diff_evsi), color="black", size=2)+
      scale_y_continuous(breaks=c(-15:5))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_EVSI_entre_diplomes_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ESPERANCE DE RETRAITE, DECOMPOSITION DES ECARTS ENTRE diplome
  diff_er_long <- comparaison_entre_diplomes %>%
    filter(diplome != "Ensemble") %>%
    select(diplome, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme", y="Années",fill="Espérance de retraite, \n écart avec les diplômés \n du supérieur...")+
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliqué par la mortalité","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_diplomes, diplome != "Ensemble"), aes(x = diplome, y = diff_er), color="black", size=2)+
      scale_y_continuous(breaks=c(-15:5))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_diplomes_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ERSI, DECOMPOSITION DES ECARTS ENTRE diplome
  diff_ersi_long <- comparaison_entre_diplomes %>%
    filter(diplome != "Ensemble") %>%
    select(diplome, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_ersi_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme", y="Années",fill="Espérance de retraite \n sans invalidité, \n écart avec les diplômés \n du supérieur")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliqué par la mortalité","expliqué par la santé","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = filter(comparaison_entre_diplomes, diplome != "Ensemble"), aes(x = diplome, y = diff_ersi), color="black", size=2)+
      scale_y_continuous(breaks=c(-15:5))+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_diplomes_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## DOUBLE PEINE par PCS
  gradients_PCS <- comparaison_entre_PCS_periodes %>%
    filter(PCS != "Ensemble") %>%
    select(Sexe, periode, PCS, diff_ev, diff_evsi, diff_evsif, diff_er, diff_ersi, diff_ersif) %>%
    pivot_longer(., cols=-c(Sexe, periode, PCS), names_to="dimension",values_to="valeur") 
  
  gradients_PCS_ponctuel <- gradients_PCS %>%
    filter(periode == "2017-2019") %>%
    mutate(
      type_esperance = ifelse(dimension %in% c("diff_ev","diff_evsi","diff_evsif"), "Espérance de vie","Espérance de retraite"),
      type_esperance = factor(type_esperance, levels = c("Espérance de vie","Espérance de retraite")),
      type_limite = case_when(
        dimension %in% c("diff_ev","diff_er") ~ "totale",
        dimension %in% c("diff_evsi","diff_ersi") ~ "sans incapacité",
        dimension %in% c("diff_evsif","diff_ersif") ~ "sans incapacité forte"
      ),
      type_limite = factor(type_limite, levels = c("totale","sans incapacité","sans incapacité forte"))
    ) %>%
    select(-dimension)
  
  (ggplot(data = gradients_PCS_ponctuel, aes(x = PCS, y=valeur, color=type_limite, group=type_limite))+
    geom_point(size=2)+
    geom_line()+
    facet_grid(type_esperance~Sexe)+
    labs(caption = paste0("Source : ",nom_enquete), x =  "PCS",y ="Années", color="")+
    scale_color_brewer(palette="Paired")+
    scale_y_continuous(breaks=seq(-8,4,by=2))) %>%
    ggsave(paste0("graphes/gradients_PCS_",enquete,".png"), plot=., width=largeur, height=hauteur, unit="cm")
  
  # DOUBLE PEINE PAR DIPLÔME
  gradients_diplome <- comparaison_entre_diplomes_periodes %>%
    filter(diplome != "Ensemble") %>%
    select(Sexe, periode, diplome, diff_ev, diff_evsi, diff_evsif, diff_er, diff_ersi, diff_ersif) %>%
    pivot_longer(., cols=-c(Sexe, periode, diplome), names_to="dimension",values_to="valeur") 
  
  gradients_diplome_ponctuel <- gradients_diplome %>%
    filter(periode == "2017-2019") %>%
    mutate(
      type_esperance = ifelse(dimension %in% c("diff_ev","diff_evsi","diff_evsif"), "Espérance de vie","Espérance de retraite"),
      type_esperance = factor(type_esperance, levels = c("Espérance de vie","Espérance de retraite")),
      type_limite = case_when(
        dimension %in% c("diff_ev","diff_er") ~ "totale",
        dimension %in% c("diff_evsi","diff_ersi") ~ "sans incapacité",
        dimension %in% c("diff_evsif","diff_ersif") ~ "sans incapacité forte"
      ),
      type_limite = factor(type_limite, levels = c("totale","sans incapacité","sans incapacité forte"))
    ) %>%
    select(-dimension)
  
  (ggplot(data = gradients_diplome_ponctuel, aes(x = diplome, y=valeur, color=type_limite, group=type_limite))+
      geom_point(size=2)+
      geom_line()+
      facet_grid(type_esperance~Sexe)+
      labs(caption = paste0("Source : ",nom_enquete), x =  "Diplôme",y ="Années", color="")+
      scale_color_brewer(palette="Paired")+
      scale_y_continuous(breaks=seq(-8,4,by=2))) %>%
    ggsave(paste0("graphes/gradients_diplome_",enquete,".png"), plot=., width=largeur, height=hauteur, unit="cm")
  
  # ÉVOLUTION DES GRADIENTS selon la PCS ENTRE PÉRIODES
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
    labs(caption = paste0("Source : ",nom_enquete), x = "PCS", y = "Années", color="Période")+
    scale_color_brewer(palette = "Paired")+
    scale_y_continuous(breaks=c(-10:0))+
    facet_grid(Sexe ~ dimension)+
    theme(legend.position="bottom")
    )%>%
    ggsave(paste0("graphes/gradients_PCS_periodes_",enquete,".png"),plot=., width=largeur*1.5, height=hauteur*1.5, unit="cm")
  
  # ÉVOLUTION DES GRADIENTS selon le diplôme ENTRE PÉRIODES
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
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme", y = "Années", color="Période")+
      scale_color_brewer(palette = "Paired")+
      scale_y_continuous(breaks=c(-10:0))+
      facet_grid(Sexe ~ dimension)+
      theme(legend.position="bottom"))%>%
    ggsave(paste0("graphes/gradients_diplome_periodes_",enquete,".png"),plot=., width=largeur*1.5, height=hauteur*1.5, unit="cm")
  
  
  
  ## ÉVOLUTION DE L'EVSI ENTRE PÉRIODES, PAR PCS
  diff_evsi_long <- comparaison_entre_periodes_par_PCS %>%
    select(PCS, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_evsi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "PCS", y="Années",fill="Espérance de vie \n sans incapacité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                        labels=c("expliquée par la santé","expliquée par la mortalité","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, y = diff_evsi), color="black", size=2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_EVSI_entre_periodes_par_PCS_",enquete,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  # ÉVOLUTION DE L'ER ENTRE PÉRIODES, PAR PCS
  diff_er_long <- comparaison_entre_periodes_par_PCS %>%
    select(PCS, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "PCS", y="Années",fill="Espérance de retraite, \n évolution entre 2009-2013 \n et 2017-2019")+
      
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliquée par la mortalité","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, y = diff_er), color="black", size=2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_periodes_par_PCS_",enquete,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  ## ÉVOLUTION DE L'ERSI ENTRE PÉRIODES, PAR PCS
  diff_ersi_long <- comparaison_entre_periodes_par_PCS %>%
    select(PCS, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
    pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_ersi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "PCS", y="Années",fill="Espérance de retraite \n sans invalidité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliquée par la mortalité","expliqueé par la santé","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_PCS, aes(x = PCS, y = diff_ersi), color="black", size=2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_periodes_par_PCS_",enquete,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  ## ÉVOLUTION DE L'EVSI ENTRE PÉRIODES, PAR diplome
  diff_evsi_long <- comparaison_entre_periodes_par_diplome %>%
    select(diplome, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_evsi_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme", y="Années",fill="Espérance de vie \n sans incapacité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                        labels=c("expliquée par la santé","expliquée par la mortalité","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, y = diff_evsi), color="black", size=2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_EVSI_entre_periodes_par_diplome_",enquete,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  # ÉVOLUTION DE L'ER ENTRE PÉRIODES, PAR diplome
  diff_er_long <- comparaison_entre_periodes_par_diplome %>%
    select(diplome, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_er_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme", y="Années",fill="Espérance de retraite, \n évolution entre 2009-2013 \n et 2017-2019")+
      
      scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels=c("expliquée par la mortalité","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, y = diff_er), color="black", size=2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ER_entre_periodes_par_diplome_",enquete,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  ## ÉVOLUTION DE L'ERSI ENTRE PÉRIODES, PAR diplome
  diff_ersi_long <- comparaison_entre_periodes_par_diplome %>%
    select(diplome, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
    pivot_longer(., cols=-c(diplome, Sexe), names_to = "composant", values_to = "valeur")
  
  (ggplot()+
      geom_bar(data = diff_ersi_long, aes(x = diplome, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
               position="stack", stat="identity")+
      labs(caption = paste0("Source : ",nom_enquete), x = "Diplôme", y="Années",fill="Espérance de retraite \n sans invalidité, \n évolution entre 2009-2013 \n et 2017-2019")+
      scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels=c("expliquée par la mortalité","expliqueé par la santé","expliquée par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      geom_point(data = comparaison_entre_periodes_par_diplome, aes(x = diplome, y = diff_ersi), color="black", size=2)+
      facet_wrap(~Sexe)
  ) %>%
    ggsave(paste0("graphes/diff_ERSI_entre_periodes_par_diplome_",enquete,".png"),plot=., width=largeur*1.2, height=hauteur, unit="cm")
  
  
  ## ECARTS ENTRE SEXES, DECOMPOSITION
  ev_hommes <- comparaison_entre_sexes %>% filter(Sexe == "Hommes") %>% select(ev) %>% pull()
  
  diff_sexe_long <- comparaison_entre_sexes %>%
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
      labs(caption = paste0("Source : ",nom_enquete), x = "", y="Années",fill="Écart des femmes \n avec les hommes...")+
     scale_x_discrete(labels=c("Espérance de vie","Espérance de retraite","Espérance de vie \n sans invalidité","Espérance de retraite \n sans invalidité"))+
      scale_y_continuous(breaks = seq(-2,6, by=1))+
    scale_fill_brewer(breaks=c("survie","sante","retraite","residu"),
                        labels=c("expliqué par la mortalité","expliqué par la santé","expliqué par la retraite","Résidu"),
                        palette="YlOrBr", direction=-1)+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
      geom_point(data = filter(diff_sexe_long, is.na(composant)), aes(x = type_esperance, y = valeur), color="black", size=2)
  ) %>%
    ggsave(paste0("graphes/diff_entre_sexes_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  
  ## ESPÉRANCE DE VIE, SÉRIE TEMPORELLE
  ev_SL_long <- comparaison_entre_annees %>% select(AENQ, Sexe, ev, evsif, evsi)%>%
    rename(evsigenerale = evsi)%>%
    pivot_longer(., cols = -c(Sexe, AENQ), names_to = "type_esperance", values_to = "valeur_esperance")
  
  # Plot the data
  (ggplot(ev_SL_long, aes(x = AENQ, y = valeur_esperance, color = type_esperance)) +
    geom_line() +
    facet_wrap(~Sexe) +
    labs(caption = paste0("Source : ",nom_enquete), x =  "Année",
         y = "",
         color = "Espérance de vie à 30 ans") +
    scale_color_brewer(palette="Reds",
                       labels = c("totale",
                                  "sans incapacité forte",
                                  "sans incapacité")) +
    scale_x_continuous(breaks = seq(2008,2019, by=4))+
    scale_y_continuous(breaks = seq(35,55,by=2.5))+
    theme_minimal())%>%
    ggsave(paste0("graphes/EV_SL_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ESPERANCE DE RETRAITE, SERIE TEMPORELLE
  er_SL_long <- comparaison_entre_annees %>% select(AENQ, Sexe, er, ersif, ersi)%>%
    rename(ersigenerale = ersi)%>%
    pivot_longer(., cols = -c(Sexe, AENQ), names_to = "type_esperance", values_to = "valeur_esperance")
  
  # Plot the data
  (ggplot(er_SL_long, aes(x = AENQ, y = valeur_esperance, color = type_esperance)) +
      geom_line() +
      facet_wrap(~Sexe) +
      labs(caption = paste0("Source : ",nom_enquete), x =  "Année",
           y = "",
           color = "Espérance de retraite") +
      scale_color_brewer(palette="Reds",
                         labels = c("totale",
                                    "sans incapacité forte",
                                    "sans incapacité")) +
      scale_x_continuous(breaks = seq(2008,2019, by=4))+
      scale_y_continuous(breaks=seq(10,25,by=2))+
      theme_minimal())%>%
    ggsave(paste0("graphes/ER_SL_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ESPÉRANCE DE VIE SANS INCAPACITÉ, SÉRIE TEMPORELLE AVEC DÉCOMPOSITION
  diff_evsi_SL_long <- comparaison_entre_annees %>% select(AENQ, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu)%>%
    pivot_longer(., cols=-c(Sexe, AENQ), names_to = "composant", values_to="valeur")
  
  (ggplot() +
    geom_col(data = diff_evsi_SL_long, aes(x = AENQ, y = valeur, fill = factor(composant, levels=c("diff_evsi_residu","diff_evsi_survie","diff_evsi_sante"))), position = "stack") +  # Barres empilées
    facet_wrap(~Sexe) +
    geom_line(data = comparaison_entre_annees, aes(x = AENQ, y = diff_evsi), color = "black", alpha=0.8) +  # Point noir représentant la somme
    labs(caption = paste0("Source : ",nom_enquete), x =  "Année",
         y = "",
         fill = paste0("Écart d'EVSI \n par rapport à ",annee_min)) +
    scale_fill_brewer(palette="Reds",
                      breaks=c("diff_evsi_survie","diff_evsi_sante","diff_evsi_residu"),
                       labels = c("expliqué par la mortalité", "expliqué par la santé", "Résidu"))+
    scale_x_continuous(breaks=seq(2008,2019,by=4))+
    theme_minimal())%>%
    ggsave(paste0("graphes/diff_EVSI_SL_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ESPÉRANCE DE RETRAITE, SÉRIE TEMPORELLE AVEC DÉCOMPOSITION
  diff_er_SL_long <- comparaison_entre_annees %>% select(AENQ, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu)%>%
    pivot_longer(., cols=-c(Sexe, AENQ), names_to = "composant", values_to="valeur")
  
  (ggplot() +
      geom_col(data = diff_er_SL_long, aes(x = AENQ, y = valeur, fill = factor(composant, levels=c("diff_er_residu","diff_er_survie","diff_er_retraite"))), position = "stack") +  # Barres empilées
      facet_wrap(~Sexe) +
      geom_line(data = comparaison_entre_annees, aes(x = AENQ, y = diff_er), color = "black", alpha=0.8) +  # Point noir représentant la somme
      geom_point(data=filter(comparaison_entre_annees, AENQ %in% c(annee_min,2019)),
                 aes(x = AENQ, y=diff_er))+
      geom_label(
        data = filter(comparaison_entre_annees, AENQ == annee_min),
        aes(x = AENQ, y = diff_er, 
            label = paste("ER en",annee_min,"\n",round(er,digits=1), "ans")), 
        color = "black", label.size=0.2, fill = "white", size = 2.5, nudge_y = 0.3, nudge_x = 0.9
      ) +
      geom_label(
        data = filter(comparaison_entre_annees, AENQ == 2019),
        aes(x = AENQ, y = diff_er, 
            label = paste("ER en 2019\n",round(er,digits=1), "ans")), 
        color = "black", label.size=0.2, fill = "white", size = 2.5, nudge_y = -0.25, nudge_x = -0.9
      ) +
      labs(caption = paste0("Source : ",nom_enquete), x =  "Année",
           y = "Écart en années",
           fill = paste0("Espérance de retraite, \n écart par rapport à ",annee_min)) +
      scale_fill_brewer(palette="Reds",
                        breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                        labels = c("expliqué par la mortalité", "expliqué par la retraite", "Résidu"))+
      scale_x_continuous(breaks=seq(2008,2019,by=4))+
      scale_y_continuous(breaks=seq(-2,1.5,by=0.5))+
      theme_minimal())%>%
    ggsave(paste0("graphes/diff_ER_SL_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
  
  ## ESPÉRANCE DE RETRAITE SANS INCAPACITÉ, SÉRIE TEMPORELLE AVEC DÉCOMPOSITION
  diff_ersi_SL_long <- comparaison_entre_annees %>% select(AENQ, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu)%>%
    pivot_longer(., cols=-c(Sexe, AENQ), names_to = "composant", values_to="valeur")
  
  (ggplot() +
      geom_col(data = diff_ersi_SL_long, aes(x = AENQ, y = valeur, fill = factor(composant, levels=c("diff_ersi_residu","diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))), position = "stack") +  # Barres empilées
      facet_wrap(~Sexe) +
      geom_line(data = comparaison_entre_annees, aes(x = AENQ, y = diff_ersi), color = "black", alpha=0.8) +  # ligne noire représentant la somme
      geom_point(data=filter(comparaison_entre_annees, AENQ %in% c(annee_min,2019)),
                 aes(x = AENQ, y=diff_ersi))+
      geom_label(
        data = filter(comparaison_entre_annees, AENQ == annee_min),
        aes(x = AENQ, y = diff_ersi, 
            label = paste("ERSI en",annee_min,"\n",round(ersi,digits=1), "ans")), 
        color = "black", label.size=0.2, fill = "white", size = 2.5, nudge_y = 0.3, nudge_x = 1.1
      ) +
      geom_label(
        data = filter(comparaison_entre_annees, AENQ == 2019),
        aes(x = AENQ, y = diff_ersi, 
            label = paste("ERSI en 2019\n",round(ersi,digits=1), "ans")), 
        color = "black", label.size=0.2, fill = "white", size = 2.5, nudge_y = 0.23, nudge_x = -1.1
      ) +
      labs(caption = paste0("Source : ",nom_enquete), x =  "Année",
           y = "Écart en années",
           fill = paste0("Espérance de retraite \n sans incapacité, \n écart par rapport à ", annee_min)) +
      scale_fill_brewer(palette="Reds",
                        breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                        labels = c("expliqué par la mortalité","expliqué par la santé", "expliqué par la retraite", "Résidu"))+
      scale_x_continuous(breaks=seq(annee_min,2019,by=3))+
      scale_y_continuous(breaks=seq(-1.5,1.5,by=0.5))+
      theme_minimal())%>%
    ggsave(paste0("graphes/diff_ERSI_SL_",enquete,".png"),plot=., width=largeur, height=hauteur, unit="cm")
}