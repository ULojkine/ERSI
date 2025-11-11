library(data.table)
library(tidyverse)
library(fst)
library(dplyr)
library(rlang)
library(rstudioapi)
library(modi)
library(fixest)

weighted_mean_na_rm <- function(vecteur, poids) {
  weighted.mean(vecteur, poids, na.rm = TRUE)
}

weighted_var_na_rm <- function(vecteur, poids) {
  m <- weighted_mean_na_rm(vecteur, poids)
  n <- length(vecteur)
  m*(1-m)/n
}

var_weighted_clustered <- function(vecteur, poids, id_individuel){
  not_na <- !is.na(vecteur) & !is.na(poids) & !is.na(id_individuel) & (poids > 0)
  vecteur <- vecteur[not_na]
  poids <- poids[not_na]
  id_individuel <- id_individuel [not_na]
  if(length(vecteur) == 0){
    return(NA)
  }
  if(length(unique(vecteur)) == 1){ # si les valeurs sont constantes, la variance est nulle, on ne peut pas appliquer feols
    return(0)
  } else{
    df <- data.frame(
      variable_dependante = as.numeric(vecteur),
      poids = poids,
      id = id_individuel
    )
    coefs <- feols(variable_dependante ~ 1,
                   weights = ~poids,
                   cluster = ~id,
                   data = df)
    return(as.numeric(coefs$se)^2)
  }
}

liste_variantes <- c("SRCV",
                     "SRCV_lisse",
                     "SRCV_revenu",
                     "SRCV_retraitebrut",
                     "SRCV_recensement",
                     "enqEmploi")

rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% dirname() %>% setwd()