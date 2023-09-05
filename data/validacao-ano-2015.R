library(readxl)
library(tidyverse)
library(geobr)
library(skimr)
library(tidymodels)
library(ISLR)
library(modeldata)
library(vip)
library(ggpubr)

files_validacao <- list.files("data/Validacao/",full.names = TRUE)

grd_read <- function(arq){
  nome <- str_split(arq, "/", simplify = TRUE)[3] %>%  str_remove(".grd")
  dados <- read.table(arq, skip = 5) %>% as.tibble()
  vetor <- as.vector(as.matrix(dados))
  id <- 1:length(vetor)
  data.frame(id,nome,vetor)
}
val <- map_df(files_validacao,grd_read)

temporal <- val %>%
  filter(str_detect(nome,"^[F|U|T]")) %>%
  mutate(ano = as.numeric(paste0("20",str_sub(nome, start = 6))),
         mes = as.numeric(str_sub(nome, start=4, end = 5)),
         dia = as.numeric(str_sub(nome, start=2, end = 3)),
         nome = (str_sub(nome, start=1, end = 1))) %>%
  pivot_wider(names_from = nome,
              values_from = vetor)

spatial <- val %>%
  filter(!str_detect(nome,"^[F|U|T]")) %>%
  mutate(nome = str_remove(nome,"_EU")) %>%
  pivot_wider(names_from = nome,
              values_from = vetor)

data_set <- left_join(temporal, spatial, by="id") %>%
  mutate(data = make_date(year= ano, month=mes, day=dia),
         local = "EU") %>%
  relocate(id,data)


file_models <- list.files("models", pattern = "EU")

fco2_modelo_load <- read_rds(paste0("models/",file_models[1]))
df <- predict(fco2_modelo_load, new_data = data_set %>%
              filter(data == "2015-10-02"))

cbind(df, data_set %>%
        filter(data == "2015-11-18"))  %>%
  ggplot(aes(x=.pred, y=F)) +
  geom_point()+
  theme_bw() +
  geom_smooth(method = "lm") +
  stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))

cbind(df, data_set %>%
        filter(data == "2015-11-18")) %>%
  pull(.pred) %>% matrix(ncol=94) %>% image()

cbind(df, data_set %>%
        filter(data == "2015-11-18")) %>%
  pull(F) %>% matrix(ncol=94) %>% image()

