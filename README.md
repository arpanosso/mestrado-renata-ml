
# mestrado-renata-ml

### Carregando os pacotes exigidos

``` r
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
```

### Listando os arquivos com os mapas de cada área separadamente

``` r
files_eu <- list.files("data/EU espacial/",full.names = TRUE)
files_sp <- list.files("data/SP espacial/",full.names = TRUE)
```

### Carregando os mapa para Eucalipto

``` r
eu <- map_df(files_eu,grd_read)
eu %>% head()
#>   id  nome vetor
#> 1  1 Al_EU  2.00
#> 2  2 Al_EU  4.94
#> 3  3 Al_EU  5.14
#> 4  4 Al_EU  5.34
#> 5  5 Al_EU  5.54
#> 6  6 Al_EU  5.75
```

### Arquivo com os dados de emissão, temperatura e umidade (temporal)

``` r
temporal_eu <- eu %>% 
  filter(str_detect(nome,"^[F|U|T]")) %>% 
  mutate(numero = as.numeric(str_remove(nome,"F|T|U")),
         ano = numero %% 10000,
         mes = numero %% 1000000 %/% 10000,
         dia = numero %/% 1e6,
         nome = str_remove_all(nome,"[0-9]")) %>% 
  pivot_wider(names_from = nome, 
              values_from = vetor)


media_eu <- temporal_eu %>% 
  group_by(id, ano) %>% 
  summarise(
    fco2 = mean(F),
    ts = mean(T),
    us = mean(U)
  )
```

### Arquivo com os dados dos atributos do solo, somente geoespacializados

``` r
spatial_eu <- eu %>% 
  filter(!str_detect(nome,"^[F|U|T]")) %>% 
  mutate(nome = str_remove(nome,"_EU")) %>% 
  pivot_wider(names_from = nome, 
              values_from = vetor)
```

### Unindo as bases de dados, ou seja, repetindo os dados do solo para cada dia de avaliação

``` r
data_eu <- left_join(temporal_eu, spatial_eu, by="id") %>% 
  select(-numero) %>% 
  mutate(data = make_date(year= ano, month=mes, day=dia)) %>% 
  relocate(id,data)

data_eu_media <- left_join(media_eu, spatial_eu, by="id") %>% 
  relocate(id)
```

### Carregando os mapa para Sistema Silvipastoril

``` r
sp <- map_df(files_sp,grd_read)
```

### Arquivo com os dados de emissão, temperatura e umidade (temporal)

``` r
temporal_sp <- sp %>% 
  filter(str_detect(nome,"^[F|U|T]")) %>% 
  mutate(numero = as.numeric(str_remove(nome,"F|T|U")),
         ano = numero %% 10000,
         mes = numero %% 1000000 %/% 10000,
         dia = numero %/% 1e6,
         nome = str_remove_all(nome,"[0-9]")) %>% 
  pivot_wider(names_from = nome, 
              values_from = vetor)

media_sp <- temporal_sp %>% 
  group_by(id, ano) %>% 
  summarise(
    fco2 = mean(F),
    ts = mean(T),
    us = mean(U)
  )
```

### Arquivo com os dados dos atributos do solo, somente geoespacializados

``` r
spatial_sp <- sp %>% 
  filter(!str_detect(nome,"^[F|U|T]")) %>% 
  mutate(nome = str_remove(nome,"_SP")) %>% 
  pivot_wider(names_from = nome, 
              values_from = vetor)
```

### Unindo as bases de dados, ou seja, repetindo os dados do solo para cada dia de avaliação

``` r
data_sp <- left_join(temporal_sp, spatial_sp, by="id") %>% 
  select(-numero) %>% 
  mutate(data = make_date(year= ano, month=mes, day=dia)) %>% 
  relocate(id,data)

data_sp_media <- left_join(media_sp, spatial_sp, by="id") %>% 
  relocate(id)
```

``` r
tibble(xnames=names(data_eu), ynames=names(data_sp)) %>% 
  mutate(logic_test = xnames == ynames)
#> # A tibble: 21 × 3
#>    xnames ynames logic_test
#>    <chr>  <chr>  <lgl>     
#>  1 id     id     TRUE      
#>  2 data   data   TRUE      
#>  3 ano    ano    TRUE      
#>  4 mes    mes    TRUE      
#>  5 dia    dia    TRUE      
#>  6 F      F      TRUE      
#>  7 T      T      TRUE      
#>  8 U      U      TRUE      
#>  9 Al     Al     TRUE      
#> 10 Ca     Ca     TRUE      
#> # ℹ 11 more rows
```

### Unindo toda a base de dados

``` r
data_set <- rbind(data_eu %>% 
                    mutate(local="EU"),
                  data_sp %>% 
                    mutate(local="SP")) %>% 
  relocate(local)

data_set_media <- rbind(data_eu_media %>% 
                    mutate(local="EU"),
                  data_sp_media %>% 
                    mutate(local="SP")) %>% 
  relocate(local)
```

## Criando os preditos para cada dia - EUCALIPTO

``` r
dias <- data_eu$data %>% unique()
dis <- 100/93
grid <- expand.grid(Y=seq(0,100,dis), X=seq(0,100,dis))
for(i in seq_along(dias)){
  di <- dias[i]
  data_eu %>% filter(data == di)
  file_models_eu <- list.files("models-3", pattern = "EU")
  file_models_eu <- grep(paste0(di), file_models_eu,value = TRUE)
  
  fco2_modelo_load_dt <- read_rds(
    paste0("models-3/",grep("dt",file_models_eu,value=TRUE)
    ))
  fco2_modelo_load_rf <- read_rds(
    paste0("models-3/",grep("rf",file_models_eu,value=TRUE)
    ))
  fco2_modelo_load_xgb <- read_rds(
    paste0("models-3/",grep("xgb",file_models_eu,value=TRUE)
    ))
  
  yp_dt <- predict(fco2_modelo_load_dt, new_data = data_eu %>%
                     filter(data == di))
  yp_rf <- predict(fco2_modelo_load_rf, new_data = data_eu %>%
                     filter(data == di))
  yp_xgb <- predict(fco2_modelo_load_xgb, new_data = data_eu %>%
                 filter(data == di))

  
  mp_krg <- tibble(grid, data_eu %>%
                     filter(data == di)) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = F)) +
    scale_fill_viridis_c() +
    coord_equal()+labs(x="",y="")
  
  mp_dt <- tibble(grid, data_eu %>%
                    filter(data == di), yp_dt) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = .pred)) +
    scale_fill_viridis_c() +
    coord_equal() +labs(x="",y="")
  
  mp_rf <- tibble(grid, data_eu %>%
                    filter(data == di), yp_rf) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = .pred)) +
    scale_fill_viridis_c() +
    coord_equal() +labs(x="",y="")
  
    mp_xgb <- tibble(grid, data_eu %>%
                    filter(data == di), yp_xgb) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = .pred)) +
    scale_fill_viridis_c() +
    coord_equal() +labs(x="",y="")
  
  mp_dt_res <- tibble(grid, data_eu %>%
                    filter(data == di), yp_dt) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = res)) +
    scale_fill_viridis_c(option = "E") +
    coord_equal() +labs(x="",y="")
  
  hist_dt_res <- tibble(grid, data_eu %>%
                          filter(data == di), yp_dt) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=res, y=..density..)) +
    geom_histogram(color="black",fill="lightgray") +theme_bw()+
    coord_cartesian(xlim=c(-1,1))
  
  mp_rf_res <- tibble(grid, data_eu %>%
                    filter(data == di), yp_rf) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = res)) +
    scale_fill_viridis_c(option = "E") +
    coord_equal() +labs(x="",y="")
  
  hist_rf_res <- tibble(grid, data_eu %>%
                          filter(data == di), yp_rf) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=res, y=..density..)) +
    geom_histogram(color="black",fill="lightgray") +theme_bw()+
    coord_cartesian(xlim=c(-1,1))
  
  mp_xgb_res <- tibble(grid, data_eu %>%
                    filter(data == di), yp_xgb) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = res)) +
    scale_fill_viridis_c(option = "E") +
    coord_equal() +labs(x="",y="")
  
  hist_xgb_res <- tibble(grid, data_eu %>%
                          filter(data == di), yp_xgb) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=res, y=..density..)) +
    geom_histogram(color="black",fill="lightgray") +theme_bw()+
    coord_cartesian(xlim=c(-1,1))
    
  print(di)
  print(mp_krg) 
  print(mp_dt)
  print(mp_dt_res)
  print(hist_dt_res)
  
  print(mp_rf)
  print(mp_rf_res)
  print(hist_rf_res)
  
  print(mp_xgb)
  print(mp_xgb_res)
  print(hist_xgb_res)
  tree_fit_rpart <- extract_fit_engine(fco2_modelo_load_dt)  
  png(paste0("trees/EU_tree_",di,".png"),         # File name
       width = 1900, height = 1000)
    rpart.plot::rpart.plot(tree_fit_rpart,cex=.8)
    dev.off() 

}
#> [15:56:05] WARNING: src/learner.cc:553: 
#>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
#>   older XGBoost, please export the model by calling `Booster.save_model` from that version
#>   first, then load it back in current version. See:
#> 
#>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
#> 
#>   for more details about differences between saving model and serializing.
#> 
#> [1] "2017-06-03"
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-10.png)<!-- -->

    #> [15:56:13] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-06-10"

![](README_files/figure-gfm/unnamed-chunk-14-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-20.png)<!-- -->

    #> [15:56:22] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-03-15"

![](README_files/figure-gfm/unnamed-chunk-14-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-28.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-30.png)<!-- -->

    #> [15:56:32] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-02-17"

![](README_files/figure-gfm/unnamed-chunk-14-31.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-32.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-33.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-34.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-35.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-36.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-37.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-38.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-39.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-40.png)<!-- -->

    #> [15:56:46] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-06-17"

![](README_files/figure-gfm/unnamed-chunk-14-41.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-42.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-43.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-44.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-45.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-46.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-47.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-48.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-49.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-50.png)<!-- -->

``` r

# Definindo a base de treino e a base de teste
# locais <- data_set %>% pull(local) %>% unique()
# for(i in seq_along(locais)){
#   lo <- locais[i]
#   data <- data_set %>% filter(local == lo)
#   dias <- data$data %>% unique()
#   for(j in seq_along(dias)){
#     di <- dias[j]
#     df <- data %>% filter(data == di)
#     fco2_initial_split <- initial_split(df  %>%                                          select(-c(id,ano,mes,dia,local)) %>%                                           sample_n(trunc(nrow(df)*.31289)), prop = 0.75)
#     fco2_train <- training(fco2_initial_split)
# 
#     hist_fco2 <- fco2_train  %>%
#       ggplot(aes(x=F, y=..density..))+
#       geom_histogram(bins = 30, color="black",  fill="lightgray")+
#       geom_density(alpha=.05,fill="red")+
#       theme_bw() +
#       labs(x="FCO2", y = "Densidade",title = paste(lo,di))
#     print(hist_fco2)
# 
#     fco2_train   %>%    
#       select(where(is.numeric)) %>%
#       drop_na() %>%
#       cor()  %>%
#       corrplot::corrplot()
# 
#     fco2_recipe <- recipe(F ~ ., data = fco2_train) %>%
#       step_novel(all_nominal_predictors()) %>%
#       step_zv(all_predictors()) %>%
#       step_dummy(all_nominal_predictors())
#     bake(prep(fco2_recipe), new_data = NULL)
#     # visdat::vis_miss(bake(prep(fco2_recipe), new_data = NULL))
# 
#     fco2_resamples <- vfold_cv(fco2_train, v = 5) #<-------
#     
#     ### DECISION TREE
#     print("ARVORE DE DECISÃO")
#     fco2_dt_model <- decision_tree(
#       cost_complexity = tune(),
#       tree_depth = tune(),
#       min_n = tune()
#     )  %>%
#       set_mode("regression")  %>%
#       set_engine("rpart")
# 
#     fco2_dt_wf <- workflow()   %>%
#       add_model(fco2_dt_model) %>%
#       add_recipe(fco2_recipe)
# 
#     grid_dt <- grid_random(
#       cost_complexity(c(-6, -4)),
#       tree_depth(range = c(8, 18)),
#       min_n(range = c(42, 52)),
#       size = 2 # <-----------------------------
#     )
# 
#     fco2_dt_tune_grid <- tune_grid(
#       fco2_dt_wf,
#       resamples = fco2_resamples,
#       grid = grid_dt,
#       metrics = metric_set(rmse)
#     )
#     print(autoplot(fco2_dt_tune_grid))
# 
#     fco2_dt_best_params <- select_best(fco2_dt_tune_grid, "rmse")
#     fco2_dt_wf <- fco2_dt_wf %>% finalize_workflow(fco2_dt_best_params)
#     fco2_dt_last_fit <- last_fit(fco2_dt_wf, fco2_initial_split)
# 
#     fco2_test_preds <- bind_rows(
#       collect_predictions(fco2_dt_last_fit)  %>%   mutate(modelo = "dt")
#     )
#     pre_obs_plot <- fco2_test_preds %>%
#       ggplot(aes(x=.pred, y=F)) +
#       geom_point()+
#       theme_bw() +
#       geom_smooth(method = "lm") +
#       stat_regline_equation(ggplot2::aes(
#         label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
#       labs(title = paste(lo,di))
#     print(pre_obs_plot)
# 
#     fco2_modelo_final <- fco2_dt_wf %>% fit(df)
#     saveRDS(fco2_modelo_final,
#             paste0("models-3/fco2_modelo_dt_",lo,"_",di,".rds"))
# 
#     fco2_dt_last_fit_model <- fco2_dt_last_fit$.workflow[[1]]$fit$fit
#     vip_plot <- vip(fco2_dt_last_fit_model,
#         aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
#       theme_bw()
#     print(vip_plot)
#     da <- fco2_test_preds %>% 
#   filter(F > 0, .pred>0 )
# 
#     my_r <- cor(da$F,da$.pred)
#     my_r2 <- my_r*my_r
#     my_mse <- Metrics::mse(da$F,da$.pred)
#     my_rmse <- Metrics::rmse(da$F,
#                              da$.pred)
#     my_mae <- Metrics::mae(da$F,da$.pred)
#     my_mape <- Metrics::mape(da$F,da$.pred)*100
#     vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse, 
#                            RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
#     print(data.frame(vector_of_metrics))
#     
# 
#     # ##RANDOM FOREST
#     print("RANDOM FOREST")
#     fco2_rf_model <- rand_forest(
#       min_n = tune(),
#       mtry = tune(),
#       trees = tune()
#     )   %>%
#       set_mode("regression")  %>%
#       set_engine("randomForest")
# 
#     fco2_rf_wf <- workflow()   %>%
#       add_model(fco2_rf_model) %>%
#       add_recipe(fco2_recipe)
# 
#     grid_rf <- grid_random(
#       min_n(range = c(20, 30)),
#       mtry(range = c(5, 10)),
#       trees(range = c(100, 500) ),
#       size = 2
#     )
#     fco2_rf_tune_grid <- tune_grid(
#       fco2_rf_wf,
#       resamples = fco2_resamples,
#       grid = grid_rf,
#       metrics = metric_set(rmse)
#     )
#     print(autoplot(fco2_rf_tune_grid))
# 
#     fco2_rf_best_params <- select_best(fco2_rf_tune_grid, "rmse")
#     fco2_rf_wf <- fco2_rf_wf %>% finalize_workflow(fco2_rf_best_params)
#     fco2_rf_last_fit <- last_fit(fco2_rf_wf, fco2_initial_split)
# 
#     fco2_test_preds <- bind_rows(
#       collect_predictions(fco2_rf_last_fit)  %>%   mutate(modelo = "rf")
#     )
#     pre_obs_plot <- fco2_test_preds %>%
#       ggplot(aes(x=.pred, y=F)) +
#       geom_point()+
#       theme_bw() +
#       geom_smooth(method = "lm") +
#       stat_regline_equation(ggplot2::aes(
#         label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
#       labs(title = paste(lo,di))
#     print(pre_obs_plot)
# 
#     fco2_modelo_final <- fco2_rf_wf %>% fit(df)
#     saveRDS(fco2_modelo_final,
#             paste0("models-3/fco2_modelo_rf_",lo,"_",di,".rds"))
# 
#     fco2_rf_last_fit_model <- fco2_rf_last_fit$.workflow[[1]]$fit$fit
#     vip_plot <- vip(fco2_rf_last_fit_model,
#         aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
#       theme_bw()
#     print(vip_plot)
#         da <- fco2_test_preds %>% 
#   filter(F > 0, .pred>0 )
# 
#     my_r <- cor(da$F,da$.pred)
#     my_r2 <- my_r*my_r
#     my_mse <- Metrics::mse(da$F,da$.pred)
#     my_rmse <- Metrics::rmse(da$F,
#                              da$.pred)
#     my_mae <- Metrics::mae(da$F,da$.pred)
#     my_mape <- Metrics::mape(da$F,da$.pred)*100
#     vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse, 
#                            RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
#     print(data.frame(vector_of_metrics))
# 
#     ##XGBOOST
#     cores = 6
#     fco2_xgb_model <- boost_tree(
#       mtry = 0.8, 
#       trees = tune(), # <---------------
#       min_n = 5, 
#       tree_depth = 4,
#       loss_reduction = 0, # lambda
#       learn_rate = tune(), # epsilon
#       sample_size = 0.8
#     )  %>%   
#       set_mode("regression")  %>% 
#       set_engine("xgboost", nthread = cores, counts = FALSE)
# 
#     fco2_xgb_wf <- workflow()  %>%  
#       add_model(fco2_xgb_model) %>%  
#       add_recipe(fco2_recipe)
# 
#     grid_xgb <- expand.grid(
#       learn_rate =  c(0.05, 0.3),
#       trees = c(2, 250, 500)
#     )
#     
#     #passo 1
#     fco2_xgb_tune_grid <- tune_grid(
#       fco2_xgb_wf,
#       resamples = fco2_resamples,
#       grid = grid_xgb,
#       metrics = metric_set(rmse)
#     )
#     # print(autoplot(fco2_xgb_tune_grid))
#     fco2_xgb_select_best_passo1 <- fco2_xgb_tune_grid %>% 
#       select_best(metric = "rmse")
#     
#     # passo 2
#     fco2_xgb_model <- boost_tree(
#       mtry = 0.8,
#       trees = fco2_xgb_select_best_passo1$trees,
#       min_n = tune(),
#       tree_depth = tune(), 
#       loss_reduction = 0, 
#       learn_rate = fco2_xgb_select_best_passo1$learn_rate, 
#       sample_size = 0.8
#     ) %>% 
#       set_mode("regression")  %>% 
#       set_engine("xgboost", nthread = cores, counts = FALSE)
#     fco2_xgb_wf <- workflow() %>%  
#       add_model(fco2_xgb_model)   %>%   
#       add_recipe(fco2_recipe)
#     
#     fco2_xgb_grid <- expand.grid(
#       tree_depth = c(1, 3, 4), 
#       min_n = c(5, 30, 60)
#     )
#     
#     fco2_xgb_tune_grid <- fco2_xgb_wf   %>%   
#       tune_grid(
#         resamples =fco2_resamples,
#         grid = fco2_xgb_grid,
#         control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
#         metrics = metric_set(rmse)
#       )
#     fco2_xgb_select_best_passo2 <- fco2_xgb_tune_grid  %>%   select_best(metric = "rmse")
#       
#     # passo 3
#     fco2_xgb_model <- boost_tree(
#       mtry = 0.8,
#       trees = fco2_xgb_select_best_passo1$trees,
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
#       loss_reduction =tune(), 
#       learn_rate = fco2_xgb_select_best_passo1$learn_rate, 
#       sample_size = 0.8
#     )  %>%  
#       set_mode("regression")  %>%  
#       set_engine("xgboost", nthread = cores, counts = FALSE)
#     
#     fco2_xgb_wf <- workflow()  %>%   
#       add_model(fco2_xgb_model)  %>%   
#       add_recipe(fco2_recipe)
#     
#     #### Grid
#     fco2_xgb_grid <- expand.grid(
#       loss_reduction = c(0.01, 0.05, 1, 2, 4, 8)
#     )
#     
#     fco2_xgb_tune_grid <- fco2_xgb_wf   %>%   
#       tune_grid(
#         resamples = fco2_resamples,
#         grid = fco2_xgb_grid,
#         control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
#         metrics = metric_set(rmse)
#       )
#     fco2_xgb_select_best_passo3 <- fco2_xgb_tune_grid %>% select_best(metric = "rmse")
#     
#     # passo 4
#     fco2_xgb_model <- boost_tree(
#       mtry = tune(),
#       trees = fco2_xgb_select_best_passo1$trees,
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
#       loss_reduction = fco2_xgb_select_best_passo3$loss_reduction, 
#       learn_rate = fco2_xgb_select_best_passo1$learn_rate, 
#       sample_size = tune()
#     )%>%  
#       set_mode("regression")  |> 
#       set_engine("xgboost", nthread = cores, counts = FALSE)
#     
#       fco2_xgb_wf <- workflow()  %>%   
#       add_model(fco2_xgb_model)  %>%   
#       add_recipe(fco2_recipe)
#     
#     fco2_xgb_grid <- expand.grid(
#       sample_size = seq(0.5, 1.0, length.out = 2), ## <---
#       mtry = seq(0.1, 1.0, length.out = 2) ## <---
#     )
#     
#     fco2_xgb_tune_grid <- fco2_xgb_wf   %>%   
#       tune_grid(
#         resamples = fco2_resamples,
#         grid = fco2_xgb_grid,
#         control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
#         metrics = metric_set(rmse)
#       )
#     fco2_xgb_select_best_passo4 <- fco2_xgb_tune_grid   %>%   select_best(metric = "rmse")
#     
#     # passo 5
#     fco2_xgb_model <- boost_tree(
#       mtry = fco2_xgb_select_best_passo4$mtry,
#       trees = tune(),
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
#       loss_reduction = fco2_xgb_select_best_passo3$loss_reduction, 
#       learn_rate = tune(), 
#       sample_size = fco2_xgb_select_best_passo4$sample_size
#     )  %>%  
#       set_mode("regression")  %>%  
#       set_engine("xgboost", nthread = cores, counts = FALSE)
#     
#     
#     fco2_xgb_wf <- workflow() %>%   
#       add_model(fco2_xgb_model)  %>%   
#       add_recipe(fco2_recipe)
#     
#     
#     fco2_xgb_grid <- expand.grid(
#       learn_rate = c(0.05, 0.10, 0.15, 0.25),
#       trees = c(100, 250, 500)
#     )
#     
#     fco2_xgb_tune_grid <- fco2_xgb_wf   %>%   
#       tune_grid(
#         resamples = fco2_resamples,
#         grid = fco2_xgb_grid,
#         control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
#         metrics = metric_set(rmse)
#       )
#     fco2_xgb_select_best_passo5 <- fco2_xgb_tune_grid   %>%   select_best(metric = "rmse")
#     
#     ## modelos final desempenho
#     fco2_xgb_model <- boost_tree(
#       mtry = fco2_xgb_select_best_passo4$mtry,
#       trees = fco2_xgb_select_best_passo5$trees,
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
#       loss_reduction = fco2_xgb_select_best_passo3$loss_reduction, 
#       learn_rate = fco2_xgb_select_best_passo5$learn_rate, 
#       sample_size = fco2_xgb_select_best_passo4$sample_size
#     ) %>%  
#       set_mode("regression")  %>%  
#       set_engine("xgboost", nthread = cores, counts = FALSE)
# 
#     df_par <- data.frame(
#       mtry = fco2_xgb_select_best_passo4$mtry,
#       trees = fco2_xgb_select_best_passo5$trees,
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
#       loss_reduction = fco2_xgb_select_best_passo3$loss_reduction, 
#       learn_rate = fco2_xgb_select_best_passo5$learn_rate, 
#       sample_size = fco2_xgb_select_best_passo4$sample_size
#     )
#     fco2_xgb_wf <- fco2_xgb_wf %>% finalize_workflow(df_par) # <------
#     fco2_xgb_last_fit <- last_fit(fco2_xgb_wf, fco2_initial_split) 
#     
#     fco2_test_preds <- bind_rows(
#       collect_predictions(fco2_xgb_last_fit)  %>%   mutate(modelo = "xgb")
#     )
#     pre_obs_plot <- fco2_test_preds %>%
#       ggplot(aes(x=.pred, y=F)) +
#       geom_point()+
#       theme_bw() +
#       geom_smooth(method = "lm") +
#       stat_regline_equation(ggplot2::aes(
#         label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
#       labs(title = paste(lo,di))
#     print(pre_obs_plot)
# 
#     fco2_modelo_final <- fco2_xgb_wf %>% fit(df)
#     saveRDS(fco2_modelo_final,
#             paste0("models-3/fco2_modelo_xgb_",lo,"_",di,".rds"))
# 
#     fco2_xgb_last_fit_model <- fco2_xgb_last_fit$.workflow[[1]]$fit$fit
#     vip_plot <- vip(fco2_xgb_last_fit_model,
#         aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
#       theme_bw()
#     print(vip_plot)
#     
#         da <- fco2_test_preds %>% 
#   filter(F > 0, .pred>0 )
# 
#     my_r <- cor(da$F,da$.pred)
#     my_r2 <- my_r*my_r
#     my_mse <- Metrics::mse(da$F,da$.pred)
#     my_rmse <- Metrics::rmse(da$F,
#                              da$.pred)
#     my_mae <- Metrics::mae(da$F,da$.pred)
#     my_mape <- Metrics::mape(da$F,da$.pred)*100
#     vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse, 
#                            RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
#     print(data.frame(vector_of_metrics))
#   }
# }
```

## Criando os preditos para cada dia - SILVIPASTORIL

``` r
# 3 foi o com 51%
# 4 com o 31%
dias <- data_sp$data %>% unique()
for(i in seq_along(dias)){
  di <- dias[i]
  data_sp %>% filter(data == di)
  file_models_sp <- list.files("models-3", pattern = "SP")
  file_models_sp <- grep(paste0(di), file_models_sp,value = TRUE)
  
  fco2_modelo_load_dt <- read_rds(
    paste0("models-3/",grep("dt",file_models_sp,value=TRUE)
    ))
  fco2_modelo_load_rf <- read_rds(
    paste0("models-3/",grep("rf",file_models_sp,value=TRUE)
    ))
  fco2_modelo_load_xgb <- read_rds(
    paste0("models-3/",grep("xgb",file_models_sp,value=TRUE)
    ))
  
  yp_dt <- predict(fco2_modelo_load_dt, new_data = data_sp %>%
                     filter(data == di))
  yp_rf <- predict(fco2_modelo_load_rf, new_data = data_sp %>%
                     filter(data == di))
  yp_xgb <- predict(fco2_modelo_load_xgb, new_data = data_sp %>%
                      filter(data == di))
  dis_y <- 50/55
  dis_x <- 120/131
  grid <- expand.grid(Y=seq(0,50,dis_y), X=seq(0,120,dis_x))
  
  mp_krg <- tibble(grid, data_sp %>%
                     filter(data == di)) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = F)) +
    scale_fill_viridis_c() +
    coord_equal()+labs(x="",y="")
  
  mp_dt <- tibble(grid, data_sp %>%
                    filter(data == di), yp_dt) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = .pred)) +
    scale_fill_viridis_c() +
    coord_equal() +labs(x="",y="")
  
  mp_rf <- tibble(grid, data_sp %>%
                    filter(data == di), yp_rf) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = .pred)) +
    scale_fill_viridis_c() +
    coord_equal() +labs(x="",y="")
  
  mp_xgb <- tibble(grid, data_sp %>%
                     filter(data == di), yp_xgb) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = .pred)) +
    scale_fill_viridis_c() +
    coord_equal() +labs(x="",y="")
  
  mp_dt_res <- tibble(grid, data_sp %>%
                        filter(data == di), yp_dt) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = res)) +
    scale_fill_viridis_c(option = "E") +
    coord_equal() +labs(x="",y="")
  
  hist_dt_res <- tibble(grid, data_sp %>%
                          filter(data == di), yp_dt) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=res, y=..density..)) +
    geom_histogram(color="black",fill="lightgray") +theme_bw()+
    coord_cartesian(xlim=c(-1,1))
  
  mp_rf_res <- tibble(grid, data_sp %>%
                        filter(data == di), yp_rf) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = res)) +
    scale_fill_viridis_c(option = "E") +
    coord_equal() +labs(x="",y="")
  
  hist_rf_res <- tibble(grid, data_sp %>%
                          filter(data == di), yp_rf) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=res, y=..density..)) +
    geom_histogram(color="black",fill="lightgray") +theme_bw()+
    coord_cartesian(xlim=c(-1,1))
  
  mp_xgb_res <- tibble(grid, data_sp %>%
                         filter(data == di), yp_xgb) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = res)) +
    scale_fill_viridis_c(option = "E") +
    coord_equal() +labs(x="",y="")
  
  hist_xgb_res <- tibble(grid, data_sp %>%
                           filter(data == di), yp_xgb) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=res, y=..density..)) +
    geom_histogram(color="black",fill="lightgray") +theme_bw()+
    coord_cartesian(xlim=c(-1,1))  
  
  print(di)
  print(mp_krg) 
  print(mp_dt)
  print(mp_dt_res)
  print(hist_dt_res)
  
  print(mp_rf)
  print(mp_rf_res)
  print(hist_rf_res)
  
  print(mp_xgb)
  print(mp_xgb_res)
  print(hist_xgb_res)
  tree_fit_rpart <- extract_fit_engine(fco2_modelo_load_dt)   
  png(paste0("trees/SI_tree_",di,".png"),         # File name
      width = 1900, height = 1000)
  rpart.plot::rpart.plot(tree_fit_rpart,cex=.8)
  dev.off()
}
#> [15:57:14] WARNING: src/learner.cc:553: 
#>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
#>   older XGBoost, please export the model by calling `Booster.save_model` from that version
#>   first, then load it back in current version. See:
#> 
#>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
#> 
#>   for more details about differences between saving model and serializing.
#> 
#> [1] "2017-02-03"
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-10.png)<!-- -->

    #> [15:57:19] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-03-03"

![](README_files/figure-gfm/unnamed-chunk-15-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-20.png)<!-- -->

    #> [15:57:25] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-06-03"

![](README_files/figure-gfm/unnamed-chunk-15-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-28.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-30.png)<!-- -->

    #> [15:57:31] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-03-08"

![](README_files/figure-gfm/unnamed-chunk-15-31.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-32.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-33.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-34.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-35.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-36.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-37.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-38.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-39.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-40.png)<!-- -->

    #> [15:57:41] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-02-09"

![](README_files/figure-gfm/unnamed-chunk-15-41.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-42.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-43.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-44.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-45.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-46.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-47.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-48.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-49.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-50.png)<!-- -->

    #> [15:57:48] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-06-10"

![](README_files/figure-gfm/unnamed-chunk-15-51.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-52.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-53.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-54.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-55.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-56.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-57.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-58.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-59.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-60.png)<!-- -->

    #> [15:57:56] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-03-17"

![](README_files/figure-gfm/unnamed-chunk-15-61.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-62.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-63.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-64.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-65.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-66.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-67.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-68.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-69.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-70.png)<!-- -->

    #> [15:58:03] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-06-17"

![](README_files/figure-gfm/unnamed-chunk-15-71.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-72.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-73.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-74.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-75.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-76.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-77.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-78.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-79.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-80.png)<!-- -->

    #> [15:58:10] WARNING: src/learner.cc:553: 
    #>   If you are loading a serialized model (like pickle in Python, RDS in R) generated by
    #>   older XGBoost, please export the model by calling `Booster.save_model` from that version
    #>   first, then load it back in current version. See:
    #> 
    #>     https://xgboost.readthedocs.io/en/latest/tutorials/saving_model.html
    #> 
    #>   for more details about differences between saving model and serializing.
    #> 
    #> [1] "2017-02-22"

![](README_files/figure-gfm/unnamed-chunk-15-81.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-82.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-83.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-84.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-85.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-86.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-87.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-88.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-89.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-15-90.png)<!-- -->

## Aprendizado de Máquina - media do período estudado

``` r
# # Definindo a base de treino e a base de teste
# locais <- data_set_media %>% pull(local) %>% unique()
# for(i in seq_along(locais)){
#   lo <- locais[i]
#   df <- data_set_media %>% filter(local == lo)
#   fco2_initial_split <- initial_split(df  %>%
#                                         ungroup() %>% 
#                                         select(-c(id,local, ano))  %>%
#                                         sample_n(trunc(nrow(df)*.51)), 
#                                       prop = 0.75)
#   fco2_train <- training(fco2_initial_split)
#   
#   hist_fco2 <- fco2_train  %>%
#     ggplot(aes(x=fco2, y=..density..))+
#     geom_histogram(bins = 30, color="black",  fill="lightgray")+
#     geom_density(alpha=.05,fill="red")+
#     theme_bw() +
#     labs(x="FCO2", y = "Densidade",title = paste(lo))
#   print(hist_fco2)
#   
#   fco2_train   %>%
#     select(where(is.numeric)) %>%
#     drop_na() %>%
#     cor()  %>%
#     corrplot::corrplot()
#   
#   fco2_recipe <- recipe(fco2 ~ ., data = fco2_train) %>%
#     step_novel(all_nominal_predictors()) %>%
#     step_zv(all_predictors()) %>%
#     step_dummy(all_nominal_predictors())
#   bake(prep(fco2_recipe), new_data = NULL)
#   # visdat::vis_miss(bake(prep(fco2_recipe), new_data = NULL))
#   
#   fco2_resamples <- vfold_cv(fco2_train, v = 5) #<-------
#   
#   ### DECISION TREE
#   print("ARVORE DE DECISÃO")
#   fco2_dt_model <- decision_tree(
#     cost_complexity = tune(),
#     tree_depth = tune(),
#     min_n = tune()
#   )  %>%
#     set_mode("regression")  %>%
#     set_engine("rpart")
#   
#   fco2_dt_wf <- workflow()   %>%
#     add_model(fco2_dt_model) %>%
#     add_recipe(fco2_recipe)
#   
#   grid_dt <- grid_random(
#     cost_complexity(c(-6, -4)),
#     tree_depth(range = c(8, 18)),
#     min_n(range = c(42, 52)),
#     size = 2 # <-----------------------------
#   )
#   
#   fco2_dt_tune_grid <- tune_grid(
#     fco2_dt_wf,
#     resamples = fco2_resamples,
#     grid = grid_dt,
#     metrics = metric_set(rmse)
#   )
#   print(autoplot(fco2_dt_tune_grid))
#   
#   fco2_dt_best_params <- select_best(fco2_dt_tune_grid, "rmse")
#   fco2_dt_wf <- fco2_dt_wf %>% finalize_workflow(fco2_dt_best_params)
#   fco2_dt_last_fit <- last_fit(fco2_dt_wf, fco2_initial_split)
#   
#   fco2_test_preds <- bind_rows(
#     collect_predictions(fco2_dt_last_fit)  %>%   mutate(modelo = "dt")
#   )
#   pre_obs_plot <- fco2_test_preds %>%
#     ggplot(aes(x=.pred, y=fco2)) +
#     geom_point()+
#     theme_bw() +
#     geom_smooth(method = "lm") +
#     stat_regline_equation(ggplot2::aes(
#       label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
#     labs(title = paste(lo))
#   print(pre_obs_plot)
#   
#   fco2_modelo_final <- fco2_dt_wf %>% fit(df)
#   saveRDS(fco2_modelo_final,
#           paste0("models-5/fco2_modelo_dt_",lo,".rds"))
#   
#   fco2_dt_last_fit_model <- fco2_dt_last_fit$.workflow[[1]]$fit$fit
#   vip_plot <- vip(fco2_dt_last_fit_model,
#                   aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
#     theme_bw()
#   print(vip_plot)
#   da <- fco2_test_preds %>%
#     filter(fco2 > 0, .pred>0 )
#   
#   my_r <- cor(da$fco2,da$.pred)
#   my_r2 <- my_r*my_r
#   my_mse <- Metrics::mse(da$fco2,da$.pred)
#   my_rmse <- Metrics::rmse(da$fco2,
#                            da$.pred)
#   my_mae <- Metrics::mae(da$fco2,da$.pred)
#   my_mape <- Metrics::mape(da$fco2,da$.pred)*100
#   vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse,
#                          RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
#   print(data.frame(vector_of_metrics))
# 
# 
# # ##RANDOM FOREST
#     print("RANDOM FOREST")
#     fco2_rf_model <- rand_forest(
#       min_n = tune(),
#       mtry = tune(),
#       trees = tune()
#     )   %>%
#       set_mode("regression")  %>%
#       set_engine("randomForest")
# 
#     fco2_rf_wf <- workflow()   %>%
#       add_model(fco2_rf_model) %>%
#       add_recipe(fco2_recipe)
# 
#     grid_rf <- grid_random(
#       min_n(range = c(20, 30)),
#       mtry(range = c(5, 10)),
#       trees(range = c(100, 500) ),
#       size = 2
#     )
#     fco2_rf_tune_grid <- tune_grid(
#       fco2_rf_wf,
#       resamples = fco2_resamples,
#       grid = grid_rf,
#       metrics = metric_set(rmse)
#     )
#     print(autoplot(fco2_rf_tune_grid))
# 
#     fco2_rf_best_params <- select_best(fco2_rf_tune_grid, "rmse")
#     fco2_rf_wf <- fco2_rf_wf %>% finalize_workflow(fco2_rf_best_params)
#     fco2_rf_last_fit <- last_fit(fco2_rf_wf, fco2_initial_split)
# 
#     fco2_test_preds <- bind_rows(
#       collect_predictions(fco2_rf_last_fit)  %>%   mutate(modelo = "rf")
#     )
#     pre_obs_plot <- fco2_test_preds %>%
#       ggplot(aes(x=.pred, y=fco2)) +
#       geom_point()+
#       theme_bw() +
#       geom_smooth(method = "lm") +
#       stat_regline_equation(ggplot2::aes(
#         label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
#       labs(title = paste(lo))
#     print(pre_obs_plot)
# 
#     fco2_modelo_final <- fco2_rf_wf %>% fit(df)
#     saveRDS(fco2_modelo_final,
#             paste0("models-5/fco2_modelo_rf_",lo,".rds"))
#     
#     fco2_rf_last_fit_model <- fco2_rf_last_fit$.workflow[[1]]$fit$fit
#     vip_plot <- vip(fco2_rf_last_fit_model,
#                     aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
#       theme_bw()
#     print(vip_plot)
#     da <- fco2_test_preds %>%
#       filter(fco2 > 0, .pred>0 )
# 
#     my_r <- cor(da$fco2,da$.pred)
#     my_r2 <- my_r*my_r
#     my_mse <- Metrics::mse(da$fco2,da$.pred)
#     my_rmse <- Metrics::rmse(da$fco2,
#                              da$.pred)
#     my_mae <- Metrics::mae(da$fco2,da$.pred)
#     my_mape <- Metrics::mape(da$fco2,da$.pred)*100
#     vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse,
#                            RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
#     print(data.frame(vector_of_metrics))
# 
#     ##XGBOOST
#     print("XGBOOST")
#     cores = 6
#     fco2_xgb_model <- boost_tree(
#       mtry = 0.8,
#       trees = tune(), # <---------------
#       min_n = 5,
#       tree_depth = 4,
#       loss_reduction = 0, # lambda
#       learn_rate = tune(), # epsilon
#       sample_size = 0.8
#     )  %>%
#       set_mode("regression")  %>%
#       set_engine("xgboost", nthread = cores, counts = FALSE)
# 
#     fco2_xgb_wf <- workflow()  %>%
#       add_model(fco2_xgb_model) %>%
#       add_recipe(fco2_recipe)
# 
#     grid_xgb <- expand.grid(
#       learn_rate =  c(0.05, 0.3),
#       trees = c(2, 250, 500)
#     )
# 
#     #passo 1
#     fco2_xgb_tune_grid <- tune_grid(
#       fco2_xgb_wf,
#       resamples = fco2_resamples,
#       grid = grid_xgb,
#       metrics = metric_set(rmse)
#     )
#     # print(autoplot(fco2_xgb_tune_grid))
#     fco2_xgb_select_best_passo1 <- fco2_xgb_tune_grid %>%
#       select_best(metric = "rmse")
# 
#     # passo 2
#     fco2_xgb_model <- boost_tree(
#       mtry = 0.8,
#       trees = fco2_xgb_select_best_passo1$trees,
#       min_n = tune(),
#       tree_depth = tune(),
#       loss_reduction = 0,
#       learn_rate = fco2_xgb_select_best_passo1$learn_rate,
#       sample_size = 0.8
#     ) %>%
#       set_mode("regression")  %>%
#       set_engine("xgboost", nthread = cores, counts = FALSE)
#     fco2_xgb_wf <- workflow() %>%
#       add_model(fco2_xgb_model)   %>%
#       add_recipe(fco2_recipe)
# 
#     fco2_xgb_grid <- expand.grid(
#       tree_depth = c(1, 3, 4),
#       min_n = c(5, 30, 60)
#     )
# 
#     fco2_xgb_tune_grid <- fco2_xgb_wf   %>%
#       tune_grid(
#         resamples =fco2_resamples,
#         grid = fco2_xgb_grid,
#         control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
#         metrics = metric_set(rmse)
#       )
#     fco2_xgb_select_best_passo2 <- fco2_xgb_tune_grid  %>%   select_best(metric = "rmse")
# 
#     # passo 3
#     fco2_xgb_model <- boost_tree(
#       mtry = 0.8,
#       trees = fco2_xgb_select_best_passo1$trees,
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth,
#       loss_reduction =tune(),
#       learn_rate = fco2_xgb_select_best_passo1$learn_rate,
#       sample_size = 0.8
#     )  %>%
#       set_mode("regression")  %>%
#       set_engine("xgboost", nthread = cores, counts = FALSE)
# 
#     fco2_xgb_wf <- workflow()  %>%
#       add_model(fco2_xgb_model)  %>%
#       add_recipe(fco2_recipe)
# 
#     #### Grid
#     fco2_xgb_grid <- expand.grid(
#       loss_reduction = c(0.01, 0.05, 1, 2, 4, 8)
#     )
# 
#     fco2_xgb_tune_grid <- fco2_xgb_wf   %>%
#       tune_grid(
#         resamples = fco2_resamples,
#         grid = fco2_xgb_grid,
#         control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
#         metrics = metric_set(rmse)
#       )
#     fco2_xgb_select_best_passo3 <- fco2_xgb_tune_grid %>% select_best(metric = "rmse")
# 
#     # passo 4
#     fco2_xgb_model <- boost_tree(
#       mtry = tune(),
#       trees = fco2_xgb_select_best_passo1$trees,
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth,
#       loss_reduction = fco2_xgb_select_best_passo3$loss_reduction,
#       learn_rate = fco2_xgb_select_best_passo1$learn_rate,
#       sample_size = tune()
#     )%>%
#       set_mode("regression")  |>
#       set_engine("xgboost", nthread = cores, counts = FALSE)
# 
#       fco2_xgb_wf <- workflow()  %>%
#       add_model(fco2_xgb_model)  %>%
#       add_recipe(fco2_recipe)
# 
#     fco2_xgb_grid <- expand.grid(
#       sample_size = seq(0.5, 1.0, length.out = 2), ## <---
#       mtry = seq(0.1, 1.0, length.out = 2) ## <---
#     )
# 
#     fco2_xgb_tune_grid <- fco2_xgb_wf   %>%
#       tune_grid(
#         resamples = fco2_resamples,
#         grid = fco2_xgb_grid,
#         control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
#         metrics = metric_set(rmse)
#       )
#     fco2_xgb_select_best_passo4 <- fco2_xgb_tune_grid   %>%   select_best(metric = "rmse")
# 
#     # passo 5
#     fco2_xgb_model <- boost_tree(
#       mtry = fco2_xgb_select_best_passo4$mtry,
#       trees = tune(),
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth,
#       loss_reduction = fco2_xgb_select_best_passo3$loss_reduction,
#       learn_rate = tune(),
#       sample_size = fco2_xgb_select_best_passo4$sample_size
#     )  %>%
#       set_mode("regression")  %>%
#       set_engine("xgboost", nthread = cores, counts = FALSE)
# 
# 
#     fco2_xgb_wf <- workflow() %>%
#       add_model(fco2_xgb_model)  %>%
#       add_recipe(fco2_recipe)
# 
# 
#     fco2_xgb_grid <- expand.grid(
#       learn_rate = c(0.05, 0.10, 0.15, 0.25),
#       trees = c(100, 250, 500)
#     )
# 
#     fco2_xgb_tune_grid <- fco2_xgb_wf   %>%
#       tune_grid(
#         resamples = fco2_resamples,
#         grid = fco2_xgb_grid,
#         control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
#         metrics = metric_set(rmse)
#       )
#     fco2_xgb_select_best_passo5 <- fco2_xgb_tune_grid   %>%   select_best(metric = "rmse")
# 
#     ## modelos final desempenho
#     fco2_xgb_model <- boost_tree(
#       mtry = fco2_xgb_select_best_passo4$mtry,
#       trees = fco2_xgb_select_best_passo5$trees,
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth,
#       loss_reduction = fco2_xgb_select_best_passo3$loss_reduction,
#       learn_rate = fco2_xgb_select_best_passo5$learn_rate,
#       sample_size = fco2_xgb_select_best_passo4$sample_size
#     ) %>%
#       set_mode("regression")  %>%
#       set_engine("xgboost", nthread = cores, counts = FALSE)
# 
#     df_par <- data.frame(
#       mtry = fco2_xgb_select_best_passo4$mtry,
#       trees = fco2_xgb_select_best_passo5$trees,
#       min_n = fco2_xgb_select_best_passo2$min_n,
#       tree_depth = fco2_xgb_select_best_passo2$tree_depth,
#       loss_reduction = fco2_xgb_select_best_passo3$loss_reduction,
#       learn_rate = fco2_xgb_select_best_passo5$learn_rate,
#       sample_size = fco2_xgb_select_best_passo4$sample_size
#     )
#     fco2_xgb_wf <- fco2_xgb_wf %>% finalize_workflow(df_par) # <------
#     fco2_xgb_last_fit <- last_fit(fco2_xgb_wf, fco2_initial_split)
# 
#     fco2_test_preds <- bind_rows(
#       collect_predictions(fco2_xgb_last_fit)  %>%   mutate(modelo = "xgb")
#     )
#     pre_obs_plot <- fco2_test_preds %>%
#       ggplot(aes(x=.pred, y=fco2)) +
#       geom_point()+
#       theme_bw() +
#       geom_smooth(method = "lm") +
#       stat_regline_equation(ggplot2::aes(
#         label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
#       labs(title = paste(lo))
#     print(pre_obs_plot)
# 
#     fco2_modelo_final <- fco2_xgb_wf %>% fit(df)
#     saveRDS(fco2_modelo_final,
#             paste0("models-5/fco2_modelo_xgb_",lo,".rds"))
# 
#     fco2_xgb_last_fit_model <- fco2_xgb_last_fit$.workflow[[1]]$fit$fit
#     vip_plot <- vip(fco2_xgb_last_fit_model,
#         aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
#       theme_bw()
#     print(vip_plot)
# 
#         da <- fco2_test_preds %>%
#   filter(fco2 > 0, .pred>0 )
# 
#     my_r <- cor(da$fco2,da$.pred)
#     my_r2 <- my_r*my_r
#     my_mse <- Metrics::mse(da$fco2,da$.pred)
#     my_rmse <- Metrics::rmse(da$fco2,
#                              da$.pred)
#     my_mae <- Metrics::mae(da$fco2,da$.pred)
#     my_mape <- Metrics::mape(da$fco2,da$.pred)*100
#     vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse,
#                            RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
#     print(data.frame(vector_of_metrics))
# }
```

# Material de Apêndice - Sinais (mapas dos atributos geoespacializados)

## Mapas Eucalipto

``` r
# for(i in seq(files_eu)){
#   mp<-read.table(files_eu[i],skip = 5)
#   image(mp %>%  as.matrix(),xlab = files_eu[i])
# }
```

## Mapas Silvipastoril

``` r
# for(i in seq(files_sp)){
#   mp<-read.table(files_sp[i],skip = 5)
#   image(mp %>%  as.matrix(),xlab = files_sp[i])
# }
```

## Análise para a média de emissão no período

### Eucalipto

``` r
dis <- 100/93
grid <- expand.grid(Y=seq(0,100,dis), X=seq(0,100,dis))

  file_models_eu <- list.files("models-5", pattern = "EU")
  # file_models_eu <- grep(paste0(di), file_models_eu,value = TRUE)
  
  # fco2_modelo_load_dt <- read_rds(
  #   paste0("models-3/",grep("dt",file_models_eu,value=TRUE)
  #   ))
  fco2_modelo_load_rf <- read_rds(
    paste0("models-5/",grep("rf",file_models_eu,value=TRUE)
    ))
  # fco2_modelo_load_xgb <- read_rds(
  #   paste0("models-3/",grep("xgb",file_models_eu,value=TRUE)
  #   ))
  
  # yp_dt <- predict(fco2_modelo_load_dt, new_data = data_eu %>%
  #                    filter(data == di))
  yp_rf <- predict(fco2_modelo_load_rf, new_data = data_eu_media)
  # yp_xgb <- predict(fco2_modelo_load_xgb, new_data = data_eu %>%
  #                filter(data == di))

  
  mp_krg <- tibble(grid, data_eu_media) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = fco2)) +
    scale_fill_viridis_c() +
    coord_equal()+labs(x="",y="")
  

  mp_rf <- tibble(grid, data_eu_media, yp_rf) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = .pred)) +
    scale_fill_viridis_c() +
    coord_equal() +labs(x="",y="")
  

  # mp_dt_res <- tibble(grid, data_eu %>%
  #                   filter(data == di), yp_dt) %>%
  #   mutate(res = F - .pred) %>% 
  #   ggplot(aes(x=X,y=Y)) +
  #   geom_tile(aes(fill = res)) +
  #   scale_fill_viridis_c(option = "E") +
  #   coord_equal() +labs(x="",y="")
  # 
  # hist_dt_res <- tibble(grid, data_eu %>%
  #                         filter(data == di), yp_dt) %>%
  #   mutate(res = F - .pred) %>% 
  #   ggplot(aes(x=res, y=..density..)) +
  #   geom_histogram(color="black",fill="lightgray") +theme_bw()+
  #   coord_cartesian(xlim=c(-1,1))
  
  mp_rf_res <- tibble(grid, data_eu_media, yp_rf) %>%
    mutate(res = (fco2 - .pred)^2) %>% 
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = res)) +
    scale_fill_viridis_c(option = "E") +
    coord_equal() +labs(x="",y="")
  
  hist_rf_res <- tibble(grid, data_eu_media, yp_rf) %>%
    mutate(res = (fco2 - .pred)^2) %>% 
    ggplot(aes(x=res, y=..density..)) +
    geom_histogram(color="black",fill="lightgray") +theme_bw()+
    coord_cartesian(xlim=c(-1,1))
  
  # mp_xgb_res <- tibble(grid, data_eu %>%
  #                   filter(data == di), yp_xgb) %>%
  #   mutate(res = F - .pred) %>% 
  #   ggplot(aes(x=X,y=Y)) +
  #   geom_tile(aes(fill = res)) +
  #   scale_fill_viridis_c(option = "E") +
  #   coord_equal() +labs(x="",y="")
  # 
  # hist_xgb_res <- tibble(grid, data_eu %>%
  #                         filter(data == di), yp_xgb) %>%
  #   mutate(res = F - .pred) %>% 
  #   ggplot(aes(x=res, y=..density..)) +
  #   geom_histogram(color="black",fill="lightgray") +theme_bw()+
  #   coord_cartesian(xlim=c(-1,1))
    
  # print(di)
  print(mp_krg) 
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
  # print(mp_dt)
  # print(mp_dt_res)
  # print(hist_dt_res)
  
  print(mp_rf)
```

![](README_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
  print(mp_rf_res)
```

![](README_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

``` r
  print(hist_rf_res)
```

![](README_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

``` r
  
  # print(mp_xgb)
  # print(mp_xgb_res)
  # print(hist_xgb_res)
  # tree_fit_rpart <- extract_fit_engine(fco2_modelo_load_dt)  
  # png(paste0("trees/EU_tree_",di,".png"),         # File name
  #      width = 1900, height = 1000)
  #   rpart.plot::rpart.plot(tree_fit_rpart,cex=.8)
  #   dev.off() 
  
  
  
  tibble(grid, data_eu_media) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = ts)) +
    scale_fill_gradient(low = "yellow", high = "blue") +
    coord_equal()+labs(x="",y="")
```

![](README_files/figure-gfm/unnamed-chunk-19-5.png)<!-- -->

### Silvipastoril

``` r
# 3 foi o com 51%
# 4 com o 31%

  file_models_sp <- list.files("models-5", pattern = "SP")
  # file_models_sp <- grep(paste0(di), file_models_sp,value = TRUE)
  
  # fco2_modelo_load_dt <- read_rds(
  #   paste0("models-3/",grep("dt",file_models_sp,value=TRUE)
  #   ))
  fco2_modelo_load_rf <- read_rds(
    paste0("models-5/",grep("rf",file_models_sp,value=TRUE)
    ))
  # fco2_modelo_load_xgb <- read_rds(
  #   paste0("models-3/",grep("xgb",file_models_sp,value=TRUE)
  #   ))
  
  # yp_dt <- predict(fco2_modelo_load_dt, new_data = data_sp %>%
  #                    filter(data == di))
  yp_rf <- predict(fco2_modelo_load_rf, new_data = data_sp_media)
  # yp_xgb <- predict(fco2_modelo_load_xgb, new_data = data_sp %>%
  #                     filter(data == di))
  dis_y <- 50/55
  dis_x <- 120/131
  grid <- expand.grid(Y=seq(0,50,dis_y), X=seq(0,120,dis_x))
  
  mp_krg <- tibble(grid, data_sp_media) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = fco2)) +
    scale_fill_viridis_c() +
    coord_equal()+labs(x="",y="")
  
  # mp_dt <- tibble(grid, data_sp %>%
  #                   filter(data == di), yp_dt) %>%
  #   ggplot(aes(x=X,y=Y)) +
  #   geom_tile(aes(fill = .pred)) +
  #   scale_fill_viridis_c() +
  #   coord_equal() +labs(x="",y="")
  
  mp_rf <- tibble(grid, data_sp_media, yp_rf) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = .pred)) +
    scale_fill_viridis_c() +
    coord_equal() +labs(x="",y="")
  
  # mp_xgb <- tibble(grid, data_sp %>%
  #                    filter(data == di), yp_xgb) %>%
  #   ggplot(aes(x=X,y=Y)) +
  #   geom_tile(aes(fill = .pred)) +
  #   scale_fill_viridis_c() +
  #   coord_equal() +labs(x="",y="")
  
  # mp_dt_res <- tibble(grid, data_sp %>%
  #                       filter(data == di), yp_dt) %>%
  #   mutate(res = F - .pred) %>% 
  #   ggplot(aes(x=X,y=Y)) +
  #   geom_tile(aes(fill = res)) +
  #   scale_fill_viridis_c(option = "E") +
  #   coord_equal() +labs(x="",y="")
  # 
  # hist_dt_res <- tibble(grid, data_sp %>%
  #                         filter(data == di), yp_dt) %>%
  #   mutate(res = F - .pred) %>% 
  #   ggplot(aes(x=res, y=..density..)) +
  #   geom_histogram(color="black",fill="lightgray") +theme_bw()+
  #   coord_cartesian(xlim=c(-1,1))
  
  mp_rf_res <- tibble(grid, data_sp %>%
                        filter(data == di), yp_rf) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = res)) +
    scale_fill_viridis_c(option = "E") +
    coord_equal() +labs(x="",y="")
  
  hist_rf_res <- tibble(grid, data_sp %>%
                          filter(data == di), yp_rf) %>%
    mutate(res = F - .pred) %>% 
    ggplot(aes(x=res, y=..density..)) +
    geom_histogram(color="black",fill="lightgray") +theme_bw()+
    coord_cartesian(xlim=c(-1,1))
  
  # mp_xgb_res <- tibble(grid, data_sp %>%
  #                        filter(data == di), yp_xgb) %>%
  #   mutate(res = F - .pred) %>% 
  #   ggplot(aes(x=X,y=Y)) +
  #   geom_tile(aes(fill = res)) +
  #   scale_fill_viridis_c(option = "E") +
  #   coord_equal() +labs(x="",y="")
  # 
  # hist_xgb_res <- tibble(grid, data_sp %>%
  #                          filter(data == di), yp_xgb) %>%
  #   mutate(res = F - .pred) %>% 
  #   ggplot(aes(x=res, y=..density..)) +
  #   geom_histogram(color="black",fill="lightgray") +theme_bw()+
  #   coord_cartesian(xlim=c(-1,1))  
  
  # print(di)
  # print(mp_krg) 
  # print(mp_dt)
  # print(mp_dt_res)
  # print(hist_dt_res)
  
  print(mp_rf)
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
  print(mp_rf_res)
```

![](README_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

``` r
  print(hist_rf_res)
```

![](README_files/figure-gfm/unnamed-chunk-20-3.png)<!-- -->

``` r
  
  # print(mp_xgb)
  # print(mp_xgb_res)
  # print(hist_xgb_res)
  # tree_fit_rpart <- extract_fit_engine(fco2_modelo_load_dt)   
  # png(paste0("trees/SI_tree_",di,".png"),         # File name
  #     width = 1900, height = 1000)
  # rpart.plot::rpart.plot(tree_fit_rpart,cex=.8)
  # dev.off()

  tibble(grid, data_sp_media, yp_rf) %>%
    ggplot(aes(x=X,y=Y)) +
    geom_tile(aes(fill = Ca)) +
    scale_fill_gradient(low = "yellow", high = "blue") +
    coord_equal() +labs(x="",y="")
```

![](README_files/figure-gfm/unnamed-chunk-20-4.png)<!-- -->
