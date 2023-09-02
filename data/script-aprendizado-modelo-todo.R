library(readxl)
library(tidyverse)
library(geobr)
library(skimr)
library(tidymodels)
library(ISLR)
library(modeldata)
library(vip)
library(ggpubr)

# files_eu <- list.files("data/EU espacial/",full.names = TRUE)
# files_sp <- list.files("data/SP espacial/",full.names = TRUE)
#
# grd_read <- function(arq){
#   nome <- str_split(arq, "/", simplify = TRUE)[3] %>%  str_remove(".grd")
#   dados <- read.table(arq, skip = 5) %>% as.tibble()
#   vetor <- as.vector(as.matrix(dados))
#   id <- 1:length(vetor)
#   data.frame(id,nome,vetor)
# }
# eu <- map_df(files_eu,grd_read)
#
# temporal_eu <- eu %>%
#   filter(str_detect(nome,"^[F|U|T]")) %>%
#   mutate(numero = as.numeric(str_remove(nome,"F|T|U")),
#          ano = numero %% 10000,
#          mes = numero %% 1000000 %/% 10000,
#          dia = numero %/% 1e6,
#          nome = str_remove_all(nome,"[0-9]")) %>%
#   pivot_wider(names_from = nome,
#               values_from = vetor)
#
# spatial_eu <- eu %>%
#   filter(!str_detect(nome,"^[F|U|T]")) %>%
#   mutate(nome = str_remove(nome,"_EU")) %>%
#   pivot_wider(names_from = nome,
#               values_from = vetor)
#
# data_eu <- left_join(temporal_eu, spatial_eu, by="id") %>%
#   select(-numero) %>%
#   mutate(data = make_date(year= ano, month=mes, day=dia)) %>%
#   relocate(id,data)
#
# sp <- map_df(files_sp,grd_read)
#
# temporal_sp <- sp %>%
#   filter(str_detect(nome,"^[F|U|T]")) %>%
#   mutate(numero = as.numeric(str_remove(nome,"F|T|U")),
#          ano = numero %% 10000,
#          mes = numero %% 1000000 %/% 10000,
#          dia = numero %/% 1e6,
#          nome = str_remove_all(nome,"[0-9]")) %>%
#   pivot_wider(names_from = nome,
#               values_from = vetor)
#
#
# spatial_sp <- sp %>%
#   filter(!str_detect(nome,"^[F|U|T]")) %>%
#   mutate(nome = str_remove(nome,"_SP")) %>%
#   pivot_wider(names_from = nome,
#               values_from = vetor)
#
# data_sp <- left_join(temporal_sp, spatial_sp, by="id") %>%
#   select(-numero) %>%
#   mutate(data = make_date(year= ano, month=mes, day=dia)) %>%
#   relocate(id,data)
#
# names(data_eu) == names(data_sp)
#
# data_set <- rbind(data_eu %>%
#                     mutate(local="EU"),
#                   data_sp %>%
#                     mutate(local="SP")) %>%
#   relocate(local)
# writexl::write_xlsx(data_set,"data/dados-krg-total.xlsx")
data_set <- readxl::read_excel("data/dados-krg-total.xlsx") #%>% sample_n(100)
glimpse(data_set)

# fco2_initial_split <- initial_split(data_set  %>%
#                                       select(-id),
#                                     prop = 0.75)
#
# fco2_train <- training(fco2_initial_split)
#
# fco2_train  %>%
#   ggplot(aes(x=F, y=..density..))+
#   geom_histogram(bins = 30, color="black",  fill="lightgray")+
#   geom_density(alpha=.05,fill="red")+
#   theme_bw() +
#   labs(x="FCO2", y = "Densidade")
#
#
# fco2_train   %>%    select(-c(ano,mes,dia,data)) %>%
#   select(where(is.numeric)) %>%
#   drop_na() %>%
#   cor()  %>%
#   corrplot::corrplot()
#
# fco2_recipe <- recipe(F ~ ., data = fco2_train %>%
#                         select(-c(ano,mes,dia,data))) %>%
#   step_novel(all_nominal_predictors()) %>%
#   step_zv(all_predictors()) %>%
#   step_dummy(all_nominal_predictors())
# bake(prep(fco2_recipe), new_data = NULL)
# visdat::vis_miss(bake(prep(fco2_recipe), new_data = NULL))
#
# fco2_resamples <- vfold_cv(fco2_train, v = 5) #<-------
# grid <- grid_regular(
#   penalty(range = c(-8, 0)),
#   levels = 10 #<-------
# )
#
# fco2_rf_model <- rand_forest(
#   min_n = tune(),
#   mtry = tune(),
#   trees = tune()
# )   %>%
#   set_mode("regression")  %>%
#   set_engine("randomForest")
#
# fco2_rf_wf <- workflow()   %>%
#   add_model(fco2_rf_model) %>%
#   add_recipe(fco2_recipe)
#
# grid_rf <- grid_random(
#   min_n(range = c(20, 30)),
#   mtry(range = c(10, 20)),
#   trees(range = c(100, 300) ),
#   size = 5
# )
# fco2_rf_tune_grid <- tune_grid(
#   fco2_rf_wf,
#   resamples = fco2_resamples,
#   grid = grid_rf,
#   metrics = metric_set(rmse)
# )
# print(autoplot(fco2_rf_tune_grid))
#
# fco2_rf_best_params <- select_best(fco2_rf_tune_grid, "rmse")
# fco2_rf_wf <- fco2_rf_wf %>% finalize_workflow(fco2_rf_best_params)
# fco2_rf_last_fit <- last_fit(fco2_rf_wf, fco2_initial_split)
#
# fco2_test_preds <- bind_rows(
#   collect_predictions(fco2_rf_last_fit)  %>%   mutate(modelo = "rf")
# )
#
# fco2_test_preds %>%
#   ggplot(aes(x=.pred, y=F)) +
#   geom_point()+
#   theme_bw() +
#   geom_smooth(method = "lm") +
#   stat_regline_equation(ggplot2::aes(
#     label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))
#
#
# fco2_rf_last_fit_model <- fco2_rf_last_fit$.workflow[[1]]$fit$fit
# vip(fco2_rf_last_fit_model,
#                  aesthetics =
#        list(color = "grey35", size = 0.8, fill="orange")) +
#    theme_bw()
#
#
fco2_modelo_final <- fco2_rf_wf %>% fit(data_set)

# saveRDS(fco2_modelo_final, "fco2_modelo_final.rds")
fco2_modelo_final_load <- read_rds("fco2_modelo_final.rds")
predict(fco2_modelo_final_load, new_data = data_set)
