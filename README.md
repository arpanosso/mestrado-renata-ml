
# mestrado-renata-ml

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
```

``` r
files_eu <- list.files("data/EU espacial/",full.names = TRUE)
files_sp <- list.files("data/SP espacial/",full.names = TRUE)
```

``` r
grd_read <- function(arq){
  nome <- str_split(arq, "/", simplify = TRUE)[3] %>%  str_remove(".grd")
  dados <- read.table(arq, skip = 5) %>% as.tibble()
  vetor <- as.vector(as.matrix(dados))
  id <- 1:length(vetor)
  data.frame(id,nome,vetor)
}
eu <- map_df(files_eu,grd_read)

temporal_eu <- eu %>% 
  filter(str_detect(nome,"^[F|U|T]")) %>% 
  mutate(numero = as.numeric(str_remove(nome,"F|T|U")),
         ano = numero %% 10000,
         mes = numero %% 1000000 %/% 10000,
         dia = numero %/% 1e6,
         nome = str_remove_all(nome,"[0-9]")) %>% 
  pivot_wider(names_from = nome, 
              values_from = vetor)

spatial_eu <- eu %>% 
  filter(!str_detect(nome,"^[F|U|T]")) %>% 
  mutate(nome = str_remove(nome,"_EU")) %>% 
  pivot_wider(names_from = nome, 
              values_from = vetor)

data_eu <- left_join(temporal_eu, spatial_eu, by="id") %>% 
  select(-numero) %>% 
  mutate(data = make_date(year= ano, month=mes, day=dia)) %>% 
  relocate(id,data)
```

``` r
sp <- map_df(files_sp,grd_read)

temporal_sp <- sp %>% 
  filter(str_detect(nome,"^[F|U|T]")) %>% 
  mutate(numero = as.numeric(str_remove(nome,"F|T|U")),
         ano = numero %% 10000,
         mes = numero %% 1000000 %/% 10000,
         dia = numero %/% 1e6,
         nome = str_remove_all(nome,"[0-9]")) %>% 
  pivot_wider(names_from = nome, 
              values_from = vetor)


spatial_sp <- sp %>% 
  filter(!str_detect(nome,"^[F|U|T]")) %>% 
  mutate(nome = str_remove(nome,"_SP")) %>% 
  pivot_wider(names_from = nome, 
              values_from = vetor)

data_sp <- left_join(temporal_sp, spatial_sp, by="id") %>% 
  select(-numero) %>% 
  mutate(data = make_date(year= ano, month=mes, day=dia)) %>% 
  relocate(id,data)

names(data_eu) == names(data_sp)
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#> [16] TRUE TRUE TRUE TRUE TRUE TRUE

data_set <- rbind(data_eu %>% 
                    mutate(local="EU"),
                  data_sp %>% 
                    mutate(local="SP")) %>% 
  relocate(local)
```

## Aprendizado de Máquina

``` r
# Definindo a base de treino e a base de teste
locais <- data_set$local %>% unique()
```

``` r
for(i in seq_along(locais)){
  lo <- locais[i]
  data <- data_set %>% filter(local == lo)
  dias <- data$data %>% unique() 
  for(j in seq_along(dias)){
    di <- dias[j]
    df <- data %>% filter(data == di)
    fco2_initial_split <- initial_split(df  %>% sample_n(trunc(nrow(df)*.65)) 
                                        , prop = 0.75)
    fco2_train <- training(fco2_initial_split)
    fco2_resamples_rf <- vfold_cv(fco2_train, v = 10)
    
    hist_fco2 <- fco2_train  %>% 
      ggplot(aes(x=F, y=..density..))+
      geom_histogram(bins = 30, color="black",  fill="lightgray")+
      geom_density(alpha=.05,fill="red")+
      theme_bw() +
      labs(x="FCO2", y = "Densidade",title = paste(lo,di))
    print(hist_fco2)
    
    fco2_train   %>%    select(-c(id,ano,mes,dia,local)) %>% 
      select(where(is.numeric)) %>%
      drop_na() %>% 
      cor()  %>%  
      corrplot::corrplot()
    
    fco2_recipe <- recipe(F ~ ., data = fco2_train %>% 
                            select(-c(id,ano,mes,dia))) %>%  
      step_novel(all_nominal_predictors()) %>% 
      step_zv(all_predictors()) %>%
      step_dummy(all_nominal_predictors())
    bake(prep(fco2_recipe), new_data = NULL)
    # visdat::vis_miss(bake(prep(fco2_recipe), new_data = NULL))
    
    fco2_resamples <- vfold_cv(fco2_train, v = 5) #<-------
    grid <- grid_regular(
      penalty(range = c(-8, 0)),
      levels = 10 #<-------
    )
    
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
      mtry(range = c(10, 20)),
      trees(range = c(100, 300) ),
      size = 20
    )
    fco2_rf_tune_grid <- tune_grid(
      fco2_rf_wf,
      resamples = fco2_resamples_rf,
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
    
    fco2_rf_last_fit_model <- fco2_rf_last_fit$.workflow[[1]]$fit$fit
    vip_plot <- vip(fco2_rf_last_fit_model,
        aesthetics = list(color = "grey35", size = 0.8, fill="orange")) +
      theme_bw()
    print(vip_plot)
  }
}
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-28.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-30.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-31.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-32.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-33.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-34.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-35.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-36.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-37.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-38.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-39.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-40.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-41.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-42.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-43.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-44.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-45.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-46.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-47.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-48.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-49.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-50.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-51.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-52.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-53.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-54.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-55.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-56.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-57.png)<!-- -->

## Mapas Eucalipto

``` r
for(i in seq(files_eu)){
  mp<-read.table(files_eu[i],skip = 5)
  image(mp %>%  as.matrix(),xlab = files_eu[i])
}
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-8-28.png)<!-- -->

## Mapas Silvipastoril

``` r
for(i in seq(files_sp)){
  mp<-read.table(files_sp[i],skip = 5)
  image(mp %>%  as.matrix(),xlab = files_sp[i])
}
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-28.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-30.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-31.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-32.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-33.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-34.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-35.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-36.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-37.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-38.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-39.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-9-40.png)<!-- -->
