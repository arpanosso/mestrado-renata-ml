
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mestrado-renata-ml

``` r
library(tidyverse)
files_eu <- list.files("data/EU espacial/",full.names = TRUE)
files_sp <- list.files("data/SP espacial/",full.names = TRUE)
files_eu[1]
#> [1] "data/EU espacial/Al_EU.grd"
```

## Mapas Eucalipto

``` r
for(i in seq(files_eu)){
  mp<-read.table(files_eu[i],skip = 5)
  image(mp %>%  as.matrix(),xlab = files_eu[i])
}
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-18.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-19.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-20.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-21.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-22.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-23.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-24.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-25.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-26.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-27.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-28.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-29.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-30.png)<!-- -->

## Mapas Silvipastoril

``` r
# for(i in seq(files_sp)){
#   mp<-read.table(files_sp[i],skip = 5)
#   image(mp %>%  as.matrix(),xlab = files_sp[i])
# }
```
