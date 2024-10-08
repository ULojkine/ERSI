library(data.table)
library(tidyverse)
library(fst)
library(dplyr)
library(rlang)

weighted_mean_na_rm <- function(donnees, poids) {
  weighted.mean(donnees, poids, na.rm = TRUE)
}