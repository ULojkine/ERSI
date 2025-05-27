library(data.table)
library(tidyverse)
library(fst)
library(dplyr)
library(rlang)
library(rstudioapi)
library(modi)

weighted_mean_na_rm <- function(vecteur, poids) {
  weighted.mean(vecteur, poids, na.rm = TRUE)
}

weighted_var_na_rm <- function(vecteur, poids) {
  m <- weighted_mean_na_rm(vecteur, poids)
  n <- length(vecteur)
  m*(1-m)/n
}

rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% dirname() %>% setwd()