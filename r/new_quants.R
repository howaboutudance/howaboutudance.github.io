library(dplyr)
cutoffs = c(50000, 100000,
                    150000, 200000,
                    300000, 500000,
                    1000000, 2000000)
approxquantile <- function(vals, quants=c(0.2, 0.8)){
  return(approxfun(y=cutoffs, x=(vals %>% as.integer %>% cumsum))(sum(vals %>% as.integer) * quants))
}

quants_2015 <- function(samp){
  return(approxquantile(samp[5:12]))
}

quants_2000 <- function(samp){
  return(approxquantile(samp[3:10]))
}

approx_quants <- function(ds, func, id="cbsa10"){
  ds %>% rowwise %>% do({
         results = data.frame(.)
         qs = func(results)
         results$perc_20 = qs[1]
         results$perc_80 = qs[2]
         results
     }) %>% as.data.frame %>% select(id, perc_20, perc_80)
}

percs2000 <- approx_quants(MSAs_2000, quants_2000)
percs2015 <- approx_quants(MSAs_2015, quants_2015, id="GEO.id2")

write.csv(percs2000, "bpercentiles_2000.csv")
write.csv(percs2015, "bpercentiles_2015.csv")