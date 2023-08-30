
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mestrado-renata-ml

``` r
library(tidyverse)
files_eu <- list.files("data/EU espacial/",full.names = TRUE)
files_sp <- list.files("data/SP espacial/",full.names = TRUE)
files_eu[1]
#> [1] "data/EU espacial/Al_EU.grd"
```

``` r
grd_read <- function(arq){
  nome <- str_split(arq, "/", simplify = TRUE)[3] %>%  str_remove(".grd")
  dados <- read.table(arq, skip = 5) %>% as.tibble()
  vetor <- as.vector(as.matrix(dados))
  data.frame(nome,vetor)
}
eu <- map_df(files_eu,grd_read)
eu$nome %>% unique() %>% sort()
#>  [1] "Al_EU"     "Ca_EU"     "Ds_EU"     "EstC_EU"   "F03062017" "F10062017"
#>  [7] "F15032017" "F17022017" "F17062017" "H-Al_EU"   "HLIFS_EU"  "K_EU"     
#> [13] "Macro_EU"  "Mg_EU"     "Micro_EU"  "P_EU"      "pH_EU"     "SB_EU"    
#> [19] "T03062017" "T10062017" "T15032017" "T17022017" "T17062017" "U03062017"
#> [25] "U10062017" "U15032017" "U17022017" "U17062017"
```

``` r
sp <- map_df(files_sp,grd_read)
sp$nome %>% unique() %>% sort()
#>  [1] "Al_SP"     "Ca_SP"     "Ds_SP"     "EstC_SP"   "F03022017" "F03032017"
#>  [7] "F03062017" "F08032017" "F09022017" "F10062017" "F17032017" "F17062017"
#> [13] "F22022017" "H_Al_SP"   "HLIFS_SP"  "K_SP"      "Macro_SP"  "Mg_SP"    
#> [19] "Micro_SP"  "P_SP"      "pH_SP"     "SB_SP"     "T03022017" "T03032017"
#> [25] "T03062017" "T08032017" "T09022017" "T10062017" "T17032017" "T17062017"
#> [31] "T22022017" "U03022017" "U03032017" "U03062017" "U08032017" "U09022017"
#> [37] "U10062017" "U17032017" "U17062017" "U22022017"
```

## Mapas Eucalipto

``` r
for(i in seq(files_eu)){
  mp<-read.table(files_eu[i],skip = 5)
  image(mp %>%  as.matrix(),xlab = files_eu[i])
}
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-28.png)<!-- -->

## Mapas Silvipastoril

``` r
for(i in seq(files_sp)){
  mp<-read.table(files_sp[i],skip = 5)
  image(mp %>%  as.matrix(),xlab = files_sp[i])
}
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-28.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-30.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-31.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-32.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-33.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-34.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-35.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-36.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-37.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-38.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-39.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-40.png)<!-- -->
