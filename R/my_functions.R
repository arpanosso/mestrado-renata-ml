grd_read <- function(arq){
  nome <- str_split(arq, "/", simplify = TRUE)[3] %>%  str_remove(".grd")
  dados <- read.table(arq, skip = 5) %>% as.tibble()
  vetor <- as.vector(as.matrix(dados))
  id <- 1:length(vetor)
  data.frame(id,nome,vetor)
}


find_near_point <- function(coord_x, coord_y, my_df, type="obs"){
  #my_df <- my_data_frame
  my_df$distancia <- sqrt((my_df$X-coord_x)^2+(my_df$Y-coord_y)^2)

  if(type == "obs"){
    return(my_df[which.min(my_df$distancia),]$Fobs)
  }else{
    return(my_df[which.min(my_df$distancia),]$media_models)
  }
}
