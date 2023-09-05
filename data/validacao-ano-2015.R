library(readxl)
library(tidyverse)
library(geobr)
library(skimr)
library(tidymodels)
library(ISLR)
library(modeldata)
library(vip)
library(ggpubr)
library(patchwork)
source("R/my_functions.R")

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
        filter(data == "2015-10-02"))  %>%
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

## Objetivo é aplicar os p modelos |EU| para cada dia
## em cada dia gerar um média dos modelos gerados
## comparar com o dados de krigagem.


file_models
dias <- data_set %>%  pull(data) %>% unique()
dis <- 100/93
grid <- expand.grid(Y=seq(0,100,dis), X=seq(0,100,dis))
dados_reais <- readxl::read_xlsx("data/dados-originais.xlsx")


for(i in seq_along(dias)){
  print(i)
  df <- data_set %>%
    filter(data == dias[i])
  preds <- data.frame(Fobs = df$F)
  for(j in seq_along(file_models)){
    fco2_modelo_load <- read_rds(paste0("models/",file_models[j]))
    pred <- predict(fco2_modelo_load, new_data = df) %>% as.vector()
    preds <- cbind(preds,pred)
  }
  preds$media_models <- apply(preds[,2:6],1,median)

  plot_obs_vs_pred<-preds  %>% select(Fobs,media_models) %>%
    ggplot(aes(x=media_models, y=Fobs)) +
    geom_point()+
    theme_bw() +
    geom_smooth(method = "lm") +
    stat_regline_equation(ggplot2::aes(
      label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))
  #print(plot_obs_vs_pred)

  mp_krg <- tibble(grid, preds  %>% select(Fobs,media_models)) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = Fobs)) +
    scale_fill_viridis_c() +
    coord_equal()

  mp_media <- tibble(grid, preds  %>% select(Fobs,media_models)) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = media_models)) +
    scale_fill_viridis_c() +
    coord_equal()

  names(preds) <- c("Fobs","model1","model2","model3",
                    "model4","model5","media_models")

  mp_model1 <- tibble(grid, preds) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = model1)) +
    scale_fill_viridis_c() +
    coord_equal()

  mp_model2 <- tibble(grid, preds) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = model2)) +
    scale_fill_viridis_c() +
    coord_equal()

  mp_model3 <- tibble(grid, preds) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = model3)) +
    scale_fill_viridis_c() +
    coord_equal()

  mp_model4 <- tibble(grid, preds) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = model4)) +
    scale_fill_viridis_c() +
    coord_equal()

  mp_model5 <- tibble(grid, preds) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = model5)) +
    scale_fill_viridis_c() +
    coord_equal()


  print((mp_krg + mp_media)/ (mp_model1 + mp_model2 + mp_model3)/
          (mp_model4 | mp_model5)
        )

  vetor_x <- dados_reais$X
  vetor_y <- dados_reais$Y
  my_data_frame <- tibble(grid, preds  %>% select(Fobs,media_models))

  dia_teste <- paste0("F",
    str_sub(dias[i],9,10),
    str_sub(dias[i],6,7),
    str_sub(dias[i],3,4)
    )

  reais <- dados_reais %>%
    select(X,Y, starts_with(dia_teste))

  names(reais) <- c("X","Y","Fobs")
  graf_final <- reais %>%
  group_by(X,Y) %>%
    mutate(previsto = find_near_point(X,Y, my_data_frame,"est"),
           observado = find_near_point(X,Y, my_data_frame)) %>%
    ungroup() %>%
    ggplot(aes(x=previsto, y=Fobs )) +
     geom_point()+
     theme_bw() +
     geom_smooth(method = "lm") +
     stat_regline_equation(ggplot2::aes(
       label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))
  print(graf_final)
}


