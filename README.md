
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
```

## Aprendizado de Máquina

``` r
# Definindo a base de treino e a base de teste
locais <- data_set %>% pull(local) %>% unique()
for(i in seq_along(locais)){
  lo <- locais[i]
  data <- data_set %>% filter(local == lo)
  dias <- data$data %>% unique()
  for(j in seq_along(dias)){
    di <- dias[j]
    df <- data %>% filter(data == di)
    fco2_initial_split <- initial_split(df  %>%                                          select(-c(id,ano,mes,dia,local)) %>%                                           sample_n(trunc(nrow(df)*.51289)), prop = 0.75)
    fco2_train <- training(fco2_initial_split)

    hist_fco2 <- fco2_train  %>%
      ggplot(aes(x=F, y=..density..))+
      geom_histogram(bins = 30, color="black",  fill="lightgray")+
      geom_density(alpha=.05,fill="red")+
      theme_bw() +
      labs(x="FCO2", y = "Densidade",title = paste(lo,di))
    print(hist_fco2)

    fco2_train   %>%    
      select(where(is.numeric)) %>%
      drop_na() %>%
      cor()  %>%
      corrplot::corrplot()

    fco2_recipe <- recipe(F ~ ., data = fco2_train) %>%
      step_novel(all_nominal_predictors()) %>%
      step_zv(all_predictors()) %>%
      step_dummy(all_nominal_predictors())
    bake(prep(fco2_recipe), new_data = NULL)
    # visdat::vis_miss(bake(prep(fco2_recipe), new_data = NULL))

    fco2_resamples <- vfold_cv(fco2_train, v = 5) #<-------
    
    ### DECISION TREE
    print("ARVORE DE DECISÃO")
    fco2_dt_model <- decision_tree(
      cost_complexity = tune(),
      tree_depth = tune(),
      min_n = tune()
    )  %>%
      set_mode("regression")  %>%
      set_engine("rpart")

    fco2_dt_wf <- workflow()   %>%
      add_model(fco2_dt_model) %>%
      add_recipe(fco2_recipe)

    grid_dt <- grid_random(
      cost_complexity(c(-6, -4)),
      tree_depth(range = c(8, 18)),
      min_n(range = c(42, 52)),
      size = 2 # <-----------------------------
    )

    fco2_dt_tune_grid <- tune_grid(
      fco2_dt_wf,
      resamples = fco2_resamples,
      grid = grid_dt,
      metrics = metric_set(rmse)
    )
    print(autoplot(fco2_dt_tune_grid))

    fco2_dt_best_params <- select_best(fco2_dt_tune_grid, "rmse")
    fco2_dt_wf <- fco2_dt_wf %>% finalize_workflow(fco2_dt_best_params)
    fco2_dt_last_fit <- last_fit(fco2_dt_wf, fco2_initial_split)

    fco2_test_preds <- bind_rows(
      collect_predictions(fco2_dt_last_fit)  %>%   mutate(modelo = "dt")
    )
    pre_obs_plot <- fco2_test_preds %>%
      ggplot(aes(x=.pred, y=F)) +
      geom_point()+
      theme_bw() +
      geom_smooth(method = "lm") +
      stat_regline_equation(ggplot2::aes(
        label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
      labs(title = paste(lo,di))
    print(pre_obs_plot)

    fco2_modelo_final <- fco2_dt_wf %>% fit(df)
    saveRDS(fco2_modelo_final,
            paste0("models-3/fco2_modelo_dt_",lo,"_",di,".rds"))

    fco2_dt_last_fit_model <- fco2_dt_last_fit$.workflow[[1]]$fit$fit
    vip_plot <- vip(fco2_dt_last_fit_model,
        aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
      theme_bw()
    print(vip_plot)
    da <- fco2_test_preds %>% 
  filter(F > 0, .pred>0 )

    my_r <- cor(da$F,da$.pred)
    my_r2 <- my_r*my_r
    my_mse <- Metrics::mse(da$F,da$.pred)
    my_rmse <- Metrics::rmse(da$F,
                             da$.pred)
    my_mae <- Metrics::mae(da$F,da$.pred)
    my_mape <- Metrics::mape(da$F,da$.pred)*100
    vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse, 
                           RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
    print(data.frame(vector_of_metrics))
    

    # ##RANDOM FOREST
    print("RANDOM FOREST")
    fco2_rf_model <- rand_forest(
      min_n = tune(),
      mtry = tune(),
      trees = tune()
    )   %>%
      set_mode("regression")  %>%
      set_engine("randomForest")

    fco2_rf_wf <- workflow()   %>%
      add_model(fco2_rf_model) %>%
      add_recipe(fco2_recipe)

    grid_rf <- grid_random(
      min_n(range = c(20, 30)),
      mtry(range = c(5, 10)),
      trees(range = c(100, 500) ),
      size = 2
    )
    fco2_rf_tune_grid <- tune_grid(
      fco2_rf_wf,
      resamples = fco2_resamples,
      grid = grid_rf,
      metrics = metric_set(rmse)
    )
    print(autoplot(fco2_rf_tune_grid))

    fco2_rf_best_params <- select_best(fco2_rf_tune_grid, "rmse")
    fco2_rf_wf <- fco2_rf_wf %>% finalize_workflow(fco2_rf_best_params)
    fco2_rf_last_fit <- last_fit(fco2_rf_wf, fco2_initial_split)

    fco2_test_preds <- bind_rows(
      collect_predictions(fco2_rf_last_fit)  %>%   mutate(modelo = "rf")
    )
    pre_obs_plot <- fco2_test_preds %>%
      ggplot(aes(x=.pred, y=F)) +
      geom_point()+
      theme_bw() +
      geom_smooth(method = "lm") +
      stat_regline_equation(ggplot2::aes(
        label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
      labs(title = paste(lo,di))
    print(pre_obs_plot)

    fco2_modelo_final <- fco2_rf_wf %>% fit(df)
    saveRDS(fco2_modelo_final,
            paste0("models-3/fco2_modelo_rf_",lo,"_",di,".rds"))

    fco2_rf_last_fit_model <- fco2_rf_last_fit$.workflow[[1]]$fit$fit
    vip_plot <- vip(fco2_rf_last_fit_model,
        aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
      theme_bw()
    print(vip_plot)
        da <- fco2_test_preds %>% 
  filter(F > 0, .pred>0 )

    my_r <- cor(da$F,da$.pred)
    my_r2 <- my_r*my_r
    my_mse <- Metrics::mse(da$F,da$.pred)
    my_rmse <- Metrics::rmse(da$F,
                             da$.pred)
    my_mae <- Metrics::mae(da$F,da$.pred)
    my_mape <- Metrics::mape(da$F,da$.pred)*100
    vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse, 
                           RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
    print(data.frame(vector_of_metrics))

    ##XGBOOST
    cores = 6
    fco2_xgb_model <- boost_tree(
      mtry = 0.8, 
      trees = tune(), # <---------------
      min_n = 5, 
      tree_depth = 4,
      loss_reduction = 0, # lambda
      learn_rate = tune(), # epsilon
      sample_size = 0.8
    )  %>%   
      set_mode("regression")  %>% 
      set_engine("xgboost", nthread = cores, counts = FALSE)

    fco2_xgb_wf <- workflow()  %>%  
      add_model(fco2_xgb_model) %>%  
      add_recipe(fco2_recipe)

    grid_xgb <- expand.grid(
      learn_rate =  c(0.05, 0.3),
      trees = c(2, 250, 500)
    )
    
    #passo 1
    fco2_xgb_tune_grid <- tune_grid(
      fco2_xgb_wf,
      resamples = fco2_resamples,
      grid = grid_xgb,
      metrics = metric_set(rmse)
    )
    # print(autoplot(fco2_xgb_tune_grid))
    fco2_xgb_select_best_passo1 <- fco2_xgb_tune_grid %>% 
      select_best(metric = "rmse")
    
    # passo 2
    fco2_xgb_model <- boost_tree(
      mtry = 0.8,
      trees = fco2_xgb_select_best_passo1$trees,
      min_n = tune(),
      tree_depth = tune(), 
      loss_reduction = 0, 
      learn_rate = fco2_xgb_select_best_passo1$learn_rate, 
      sample_size = 0.8
    ) %>% 
      set_mode("regression")  %>% 
      set_engine("xgboost", nthread = cores, counts = FALSE)
    fco2_xgb_wf <- workflow() %>%  
      add_model(fco2_xgb_model)   %>%   
      add_recipe(fco2_recipe)
    
    fco2_xgb_grid <- expand.grid(
      tree_depth = c(1, 3, 4), 
      min_n = c(5, 30, 60)
    )
    
    fco2_xgb_tune_grid <- fco2_xgb_wf   %>%   
      tune_grid(
        resamples =fco2_resamples,
        grid = fco2_xgb_grid,
        control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
        metrics = metric_set(rmse)
      )
    fco2_xgb_select_best_passo2 <- fco2_xgb_tune_grid  %>%   select_best(metric = "rmse")
      
    # passo 3
    fco2_xgb_model <- boost_tree(
      mtry = 0.8,
      trees = fco2_xgb_select_best_passo1$trees,
      min_n = fco2_xgb_select_best_passo2$min_n,
      tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
      loss_reduction =tune(), 
      learn_rate = fco2_xgb_select_best_passo1$learn_rate, 
      sample_size = 0.8
    )  %>%  
      set_mode("regression")  %>%  
      set_engine("xgboost", nthread = cores, counts = FALSE)
    
    fco2_xgb_wf <- workflow()  %>%   
      add_model(fco2_xgb_model)  %>%   
      add_recipe(fco2_recipe)
    
    #### Grid
    fco2_xgb_grid <- expand.grid(
      loss_reduction = c(0.01, 0.05, 1, 2, 4, 8)
    )
    
    fco2_xgb_tune_grid <- fco2_xgb_wf   %>%   
      tune_grid(
        resamples = fco2_resamples,
        grid = fco2_xgb_grid,
        control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
        metrics = metric_set(rmse)
      )
    fco2_xgb_select_best_passo3 <- fco2_xgb_tune_grid %>% select_best(metric = "rmse")
    
    # passo 4
    fco2_xgb_model <- boost_tree(
      mtry = tune(),
      trees = fco2_xgb_select_best_passo1$trees,
      min_n = fco2_xgb_select_best_passo2$min_n,
      tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
      loss_reduction = fco2_xgb_select_best_passo3$loss_reduction, 
      learn_rate = fco2_xgb_select_best_passo1$learn_rate, 
      sample_size = tune()
    )%>%  
      set_mode("regression")  |> 
      set_engine("xgboost", nthread = cores, counts = FALSE)
    
      fco2_xgb_wf <- workflow()  %>%   
      add_model(fco2_xgb_model)  %>%   
      add_recipe(fco2_recipe)
    
    fco2_xgb_grid <- expand.grid(
      sample_size = seq(0.5, 1.0, length.out = 2), ## <---
      mtry = seq(0.1, 1.0, length.out = 2) ## <---
    )
    
    fco2_xgb_tune_grid <- fco2_xgb_wf   %>%   
      tune_grid(
        resamples = fco2_resamples,
        grid = fco2_xgb_grid,
        control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
        metrics = metric_set(rmse)
      )
    fco2_xgb_select_best_passo4 <- fco2_xgb_tune_grid   %>%   select_best(metric = "rmse")
    
    # passo 5
    fco2_xgb_model <- boost_tree(
      mtry = fco2_xgb_select_best_passo4$mtry,
      trees = tune(),
      min_n = fco2_xgb_select_best_passo2$min_n,
      tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
      loss_reduction = fco2_xgb_select_best_passo3$loss_reduction, 
      learn_rate = tune(), 
      sample_size = fco2_xgb_select_best_passo4$sample_size
    )  %>%  
      set_mode("regression")  %>%  
      set_engine("xgboost", nthread = cores, counts = FALSE)
    
    
    fco2_xgb_wf <- workflow() %>%   
      add_model(fco2_xgb_model)  %>%   
      add_recipe(fco2_recipe)
    
    
    fco2_xgb_grid <- expand.grid(
      learn_rate = c(0.05, 0.10, 0.15, 0.25),
      trees = c(100, 250, 500)
    )
    
    fco2_xgb_tune_grid <- fco2_xgb_wf   %>%   
      tune_grid(
        resamples = fco2_resamples,
        grid = fco2_xgb_grid,
        control = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
        metrics = metric_set(rmse)
      )
    fco2_xgb_select_best_passo5 <- fco2_xgb_tune_grid   %>%   select_best(metric = "rmse")
    
    ## modelos final desempenho
    fco2_xgb_model <- boost_tree(
      mtry = fco2_xgb_select_best_passo4$mtry,
      trees = fco2_xgb_select_best_passo5$trees,
      min_n = fco2_xgb_select_best_passo2$min_n,
      tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
      loss_reduction = fco2_xgb_select_best_passo3$loss_reduction, 
      learn_rate = fco2_xgb_select_best_passo5$learn_rate, 
      sample_size = fco2_xgb_select_best_passo4$sample_size
    ) %>%  
      set_mode("regression")  %>%  
      set_engine("xgboost", nthread = cores, counts = FALSE)

    df_par <- data.frame(
      mtry = fco2_xgb_select_best_passo4$mtry,
      trees = fco2_xgb_select_best_passo5$trees,
      min_n = fco2_xgb_select_best_passo2$min_n,
      tree_depth = fco2_xgb_select_best_passo2$tree_depth, 
      loss_reduction = fco2_xgb_select_best_passo3$loss_reduction, 
      learn_rate = fco2_xgb_select_best_passo5$learn_rate, 
      sample_size = fco2_xgb_select_best_passo4$sample_size
    )
    fco2_xgb_wf <- fco2_xgb_wf %>% finalize_workflow(df_par) # <------
    fco2_xgb_last_fit <- last_fit(fco2_xgb_wf, fco2_initial_split) 
    
    fco2_test_preds <- bind_rows(
      collect_predictions(fco2_xgb_last_fit)  %>%   mutate(modelo = "xgb")
    )
    pre_obs_plot <- fco2_test_preds %>%
      ggplot(aes(x=.pred, y=F)) +
      geom_point()+
      theme_bw() +
      geom_smooth(method = "lm") +
      stat_regline_equation(ggplot2::aes(
        label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")))+
      labs(title = paste(lo,di))
    print(pre_obs_plot)

    fco2_modelo_final <- fco2_xgb_wf %>% fit(df)
    saveRDS(fco2_modelo_final,
            paste0("models-3/fco2_modelo_xgb_",lo,"_",di,".rds"))

    fco2_xgb_last_fit_model <- fco2_xgb_last_fit$.workflow[[1]]$fit$fit
    vip_plot <- vip(fco2_xgb_last_fit_model,
        aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
      theme_bw()
    print(vip_plot)
    
        da <- fco2_test_preds %>% 
  filter(F > 0, .pred>0 )

    my_r <- cor(da$F,da$.pred)
    my_r2 <- my_r*my_r
    my_mse <- Metrics::mse(da$F,da$.pred)
    my_rmse <- Metrics::rmse(da$F,
                             da$.pred)
    my_mae <- Metrics::mae(da$F,da$.pred)
    my_mape <- Metrics::mape(da$F,da$.pred)*100
    vector_of_metrics <- c(r=my_r, R2=my_r2, MSE=my_mse, 
                           RMSE=my_rmse, MAE=my_mae, MAPE=my_mape)
    print(data.frame(vector_of_metrics))
  }
}
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.96524046
    #> R2          0.93168914
    #> MSE         0.03172493
    #> RMSE        0.17811493
    #> MAE         0.12042865
    #> MAPE        3.15630146
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-8.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.993642797
    #> R2         0.987326009
    #> MSE        0.006501958
    #> RMSE       0.080634719
    #> MAE        0.055129353
    #> MAPE       1.479109132

![](README_files/figure-gfm/unnamed-chunk-14-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-10.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.995602984
    #> R2         0.991225302
    #> MSE        0.004079066
    #> RMSE       0.063867566
    #> MAE        0.046851884
    #> MAPE       1.245854370

![](README_files/figure-gfm/unnamed-chunk-14-11.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-14.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.97111955
    #> R2          0.94307318
    #> MSE         0.03026605
    #> RMSE        0.17397141
    #> MAE         0.10825675
    #> MAPE        3.43045078
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-17.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.995428377
    #> R2         0.990877653
    #> MSE        0.005166783
    #> RMSE       0.071880341
    #> MAE        0.046755716
    #> MAPE       1.507643081

![](README_files/figure-gfm/unnamed-chunk-14-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-19.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.996044243
    #> R2         0.992104134
    #> MSE        0.004238968
    #> RMSE       0.065107361
    #> MAE        0.048306473
    #> MAPE       1.584811887

![](README_files/figure-gfm/unnamed-chunk-14-20.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-23.png)<!-- -->

    #>      vector_of_metrics
    #> r            0.9791578
    #> R2           0.9587501
    #> MSE          0.1306334
    #> RMSE         0.3614325
    #> MAE          0.2498442
    #> MAPE         6.5773152
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-26.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.99497434
    #> R2          0.98997394
    #> MSE         0.03399734
    #> RMSE        0.18438369
    #> MAE         0.12596408
    #> MAPE        3.38838653

![](README_files/figure-gfm/unnamed-chunk-14-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-28.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.99680631
    #> R2          0.99362282
    #> MSE         0.02025893
    #> RMSE        0.14233389
    #> MAE         0.10093043
    #> MAPE        2.69793849

![](README_files/figure-gfm/unnamed-chunk-14-29.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-30.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-31.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-32.png)<!-- -->

    #>      vector_of_metrics
    #> r            0.9399454
    #> R2           0.8834973
    #> MSE          0.3064829
    #> RMSE         0.5536090
    #> MAE          0.3625507
    #> MAPE         5.3718164
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-33.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-34.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-35.png)<!-- -->

    #>      vector_of_metrics
    #> r            0.9910449
    #> R2           0.9821701
    #> MSE          0.0550342
    #> RMSE         0.2345937
    #> MAE          0.1597102
    #> MAPE         2.4212565

![](README_files/figure-gfm/unnamed-chunk-14-36.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-37.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.99263061
    #> R2          0.98531552
    #> MSE         0.03929455
    #> RMSE        0.19822853
    #> MAE         0.14270433
    #> MAPE        2.16523663

![](README_files/figure-gfm/unnamed-chunk-14-38.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-39.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-40.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-41.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.98229706
    #> R2          0.96490752
    #> MSE         0.01235039
    #> RMSE        0.11113233
    #> MAE         0.08195672
    #> MAPE        2.04118523
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-42.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-43.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-44.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.996033575
    #> R2         0.992082882
    #> MSE        0.002989187
    #> RMSE       0.054673458
    #> MAE        0.041105743
    #> MAPE       1.029433203

![](README_files/figure-gfm/unnamed-chunk-14-45.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-46.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.996101802
    #> R2         0.992218799
    #> MSE        0.002757562
    #> RMSE       0.052512497
    #> MAE        0.040387739
    #> MAPE       1.016460934

![](README_files/figure-gfm/unnamed-chunk-14-47.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-48.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-49.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-50.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.93147838
    #> R2          0.86765196
    #> MSE         0.03685642
    #> RMSE        0.19198027
    #> MAE         0.11962073
    #> MAPE        3.78623359
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-51.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-52.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-53.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.990131587
    #> R2         0.980360560
    #> MSE        0.006195856
    #> RMSE       0.078713759
    #> MAE        0.053432289
    #> MAPE       1.674756432

![](README_files/figure-gfm/unnamed-chunk-14-54.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-55.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.989513045
    #> R2         0.979136066
    #> MSE        0.005846852
    #> RMSE       0.076464709
    #> MAE        0.054457030
    #> MAPE       1.694230862

![](README_files/figure-gfm/unnamed-chunk-14-56.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-57.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-58.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-59.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.95489870
    #> R2          0.91183152
    #> MSE         0.01841117
    #> RMSE        0.13568777
    #> MAE         0.09467923
    #> MAPE        2.09797525
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-60.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-61.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-62.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.989917602
    #> R2         0.979936858
    #> MSE        0.004595255
    #> RMSE       0.067788312
    #> MAE        0.047173951
    #> MAPE       1.051156636

![](README_files/figure-gfm/unnamed-chunk-14-63.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-64.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.991277102
    #> R2         0.982630292
    #> MSE        0.003663334
    #> RMSE       0.060525483
    #> MAE        0.044493993
    #> MAPE       0.993559371

![](README_files/figure-gfm/unnamed-chunk-14-65.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-66.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-67.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-68.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.92349771
    #> R2          0.85284802
    #> MSE         0.02179714
    #> RMSE        0.14763855
    #> MAE         0.08757442
    #> MAPE        2.61646309
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-69.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-70.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-71.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.984272185
    #> R2         0.968791735
    #> MSE        0.005094213
    #> RMSE       0.071373758
    #> MAE        0.040790118
    #> MAPE       1.212759521

![](README_files/figure-gfm/unnamed-chunk-14-72.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-73.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.987586801
    #> R2         0.975327690
    #> MSE        0.003685666
    #> RMSE       0.060709684
    #> MAE        0.040244621
    #> MAPE       1.207263383

![](README_files/figure-gfm/unnamed-chunk-14-74.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-75.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-76.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-77.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.97529260
    #> R2          0.95119566
    #> MSE         0.01434486
    #> RMSE        0.11977005
    #> MAE         0.07896086
    #> MAPE        1.87455167
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-78.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-79.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-80.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.995292130
    #> R2         0.990606424
    #> MSE        0.003102094
    #> RMSE       0.055696448
    #> MAE        0.040301584
    #> MAPE       0.954450574

![](README_files/figure-gfm/unnamed-chunk-14-81.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-82.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.994978989
    #> R2         0.989983188
    #> MSE        0.002960119
    #> RMSE       0.054406976
    #> MAE        0.038916552
    #> MAPE       0.921551793

![](README_files/figure-gfm/unnamed-chunk-14-83.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-84.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-85.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-86.png)<!-- -->

    #>      vector_of_metrics
    #> r            0.9145876
    #> R2           0.8364705
    #> MSE          0.1030590
    #> RMSE         0.3210280
    #> MAE          0.1807676
    #> MAPE         3.3186413
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-87.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-88.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-89.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.98657057
    #> R2          0.97332149
    #> MSE         0.01912989
    #> RMSE        0.13831084
    #> MAE         0.08470589
    #> MAPE        1.58783995

![](README_files/figure-gfm/unnamed-chunk-14-90.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-91.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.98950996
    #> R2          0.97912996
    #> MSE         0.01324195
    #> RMSE        0.11507368
    #> MAE         0.07097831
    #> MAPE        1.32850426

![](README_files/figure-gfm/unnamed-chunk-14-92.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-93.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-94.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-95.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.949618488
    #> R2         0.901775273
    #> MSE        0.005963251
    #> RMSE       0.077222089
    #> MAE        0.039160581
    #> MAPE       1.560899252
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-96.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-97.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-98.png)<!-- -->

    #>      vector_of_metrics
    #> r         0.9924566884
    #> R2        0.9849702783
    #> MSE       0.0009918791
    #> RMSE      0.0314941124
    #> MAE       0.0202415375
    #> MAPE      0.8089422794

![](README_files/figure-gfm/unnamed-chunk-14-99.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-100.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.988437037
    #> R2         0.977007775
    #> MSE        0.001399825
    #> RMSE       0.037414236
    #> MAE        0.027067570
    #> MAPE       1.085957842

![](README_files/figure-gfm/unnamed-chunk-14-101.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-102.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-103.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-104.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.966878660
    #> R2         0.934854343
    #> MSE        0.004645566
    #> RMSE       0.068158388
    #> MAE        0.044200021
    #> MAPE       1.003661751
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-105.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-106.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-107.png)<!-- -->

    #>      vector_of_metrics
    #> r         0.9935443100
    #> R2        0.9871302958
    #> MSE       0.0009893257
    #> RMSE      0.0314535486
    #> MAE       0.0220547888
    #> MAPE      0.5003072126

![](README_files/figure-gfm/unnamed-chunk-14-108.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-109.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.990930479
    #> R2         0.981943214
    #> MSE        0.001298142
    #> RMSE       0.036029735
    #> MAE        0.027232238
    #> MAPE       0.621138991

![](README_files/figure-gfm/unnamed-chunk-14-110.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-111.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-112.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-113.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.947523941
    #> R2         0.897801619
    #> MSE        0.005474458
    #> RMSE       0.073989577
    #> MAE        0.044391750
    #> MAPE       1.496940101
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-114.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-115.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-116.png)<!-- -->

    #>      vector_of_metrics
    #> r         0.9914792221
    #> R2        0.9830310479
    #> MSE       0.0009654777
    #> RMSE      0.0310721369
    #> MAE       0.0201836590
    #> MAPE      0.6790754991

![](README_files/figure-gfm/unnamed-chunk-14-117.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-118.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.986915759
    #> R2         0.974002716
    #> MSE        0.001397126
    #> RMSE       0.037378150
    #> MAE        0.027181511
    #> MAPE       0.920145168

![](README_files/figure-gfm/unnamed-chunk-14-119.png)<!-- -->

    #> [1] "ARVORE DE DECISÃO"

![](README_files/figure-gfm/unnamed-chunk-14-120.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-121.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-122.png)<!-- -->

    #>      vector_of_metrics
    #> r           0.92116818
    #> R2          0.84855081
    #> MSE         0.03375386
    #> RMSE        0.18372224
    #> MAE         0.12170486
    #> MAPE        2.72788257
    #> [1] "RANDOM FOREST"

![](README_files/figure-gfm/unnamed-chunk-14-123.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-124.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-125.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.987638099
    #> R2         0.975429015
    #> MSE        0.006629223
    #> RMSE       0.081420043
    #> MAE        0.055692659
    #> MAPE       1.262768560

![](README_files/figure-gfm/unnamed-chunk-14-126.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-14-127.png)<!-- -->

    #>      vector_of_metrics
    #> r          0.988563291
    #> R2         0.977257381
    #> MSE        0.005104343
    #> RMSE       0.071444688
    #> MAE        0.052905387
    #> MAPE       1.206445867

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
