library(readxl)
library(tidyverse)
library(geobr)
library(skimr)
library(tidymodels)
library(ISLR)
library(modeldata)
library(vip)
library(ggpubr)
source("R/my_functions.R")

files_eu <- list.files("data/EU espacial/",full.names = TRUE,pattern = "F")[-6]
files_sp <- list.files("data/SP espacial/",full.names = TRUE,pattern = "F")[-10]

eu <- map_df(files_eu,grd_read)
temporal_eu <- eu %>%
  filter(str_detect(nome,"^[F]")) %>%
  mutate(numero = as.numeric(str_remove(nome,"F")),
         ano = numero %% 10000,
         mes = numero %% 1000000 %/% 10000,
         dia = numero %/% 1e6,
         nome = str_remove_all(nome,"[0-9]")) %>%
  pivot_wider(names_from = nome,
              values_from = vetor)

sp <- map_df(files_sp,grd_read)

temporal_sp <- sp %>%
  filter(str_detect(nome,"^[F]")) %>%
  mutate(numero = as.numeric(str_remove(nome,"F")),
         ano = numero %% 10000,
         mes = numero %% 1000000 %/% 10000,
         dia = numero %/% 1e6,
         nome = str_remove_all(nome,"[0-9]")) %>%
  pivot_wider(names_from = nome,
              values_from = vetor)

file_models_eu <- list.files("models-3", pattern = "EU")






















