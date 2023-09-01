library(tidyverse)
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

data <- left_join(temporal, spatial, by="id") %>%
  mutate(data = make_date(year= ano, month=mes, day=dia),
         local = "EU") %>%
  relocate(id,data)
data
fco2_rf_last_fit <- read_rds("data/fco2_rf_last_fit.rds")
# data$.pred <- predict(fco2_rf_last_fit, data)
#
# predict(fco2_rf_last_fit, new_data = data, type="prob") %>%
#    arrange(desc(.pred_Yes))
# #
# table(
#   predict(telco_modelo_final, new_data = telco_test, type="prob")$.pred_Yes > 0.5,
#   telco_test$Churn
# )
