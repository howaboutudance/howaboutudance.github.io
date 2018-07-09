#' @title New Cumalative Frequency Library
#' @author Michael Penhallegon
#'
#'
#'

## use the dplyr library
library(dplyr)
library(ggplot2)

## cutoffs are the "right" side of each grouped data cutoff or partition
## this equates to ranges of:
## 0-50k
## 50k-100k
## 100k-150k
## 150k-200k
## 200k-300k
## 300k-500k
## 1 million or more (arbitarily cutoff at a 2 million)
cutoffs = c(50000, 100000,
            150000, 200000,
            300000, 500000,
            1000000, 2000000)
## 
#msas_perc2000 <- ltdb2000 %>% group_by(cbsa10) %>% 
#  summarise(msa_perc = sum(NHBLK00)/sum(POP00) * 100, pop_00 = sum(POP00)) %>% 
#  inner_join(MSAs_2015, by=c("cbsa10" = "GEO.id2")) %>% 
#  select(GEO.display.label, pop_00, cbsa10, msa_perc)

## import MSA quantiles calculated by new_cumal_freq.Rmd
bpercentiles_2000 <- read.csv("bpercentiles_2000.csv")
bpercentiles_2015 <- read.csv("bpercentiles_2015.csv")

#' calculate_ICE: calculates the ICE value using the census tract order group data
#' 
#' uses approxfun to create a linear interpolated cumalative frequency
#' curve/function and then runs the respective quantiles(qs) against. uses
#' cumsum to provide a cumalative summation of values from the grouped data
#'
#' this results in an array called ps that is then entered into the equation for ICE:
#'
#'  A_i-P_i
#'  -----
#'    n
#'
#' ps thought are left-tail counts, so for 20th percentile and below nothing
#' needs to be done. With 80th percentile right-tails, we take n (total counts)
#' and substract the 80th percentile value.
#'
#' @param ctunit a row from LTDB or Census data, should be a data.frame or tibble
#' @param qs quantiles from the respective MSA, should be a c() or other array type
#' @param groupdRef a seq of column to select from ctunit for all the values, should an array type
#' @param nref: the column reference to the n (total count) of values, should be a integer
#'
#' @return an ICE value
calculate_ICE <- function(ctunit, qs, groupdRef, nref){
  ps = approxfun(x = cutoffs, y=cumsum(ctunit[,groupdRef] %>% unlist))(qs)
  n = ctunit[nref]
  return(
    (n-ps[1] - ps[2])/n
  )
}

#'approx_warp_2000
#'
#'
#'is a wrapper function for the owner-occupied housing data
#'from the Census 2000 that I have been using
#'
#'it sets the column references for the order group frequency data, n column
#'and sets the correct quantiles based on the cbsa10 column. takes those
#'variables and call calculate_ICE and return the results ICE value
#'
#' @param ctunit a row of census tract data from the census or LTDB, can be a tibble or data.frame
#'
#' @return a calculated ICE value
approx_wrap_2000 <- function(ctunit){
  gpref = 9:16
  nref = 8
  qs = bpercentiles_2000 %>% filter(cbsa10 == ct$cbsa10) %>% select(perc_80, perc_20) %>% unlist
  return(calculate_ICE(ctunit, qs, gpref, nref))
}

#' approx_warp_2015 is a wrapper function for the owner-occupied housing data
#' from the 2015 aCS 5-year sample estimates that I have been using
#'
#' it sets the column references for the order group frequency data, n column
#' and sets the correct quantiles based on the cbsa10 column. takes those
#' variables and call calculate_ICE and return the results ICE value
#'
#' @param ctunit: a row of census tract data from the census or LTDB, can be a tibble or data.frame
#'
#' @return a calculated ICE value
approx_wrap_2015 <- function(ctunit){
  gpref = 3:10
  nref = 2
  qs = bpercentiles_2015 %>% filter(cbsa10 == ct$cbsa10) %>% select(perc_80, perc_20) %>% unlist
  return(napproxct(ctunit, qs, gpref, nref))
}

#' aggr_calculate_ICE
#'
#' calculates ICE values into data.frame from data.frames of Cenus Tract value.
#' 
#' uses a dplyr pipeline to take our import data CT_2000, and filter out all
#' census tract with 50 housing units or more. then it is broken with rowwise to
#' be sent into do{} block. inside the do block the row is converted to a
#' data.frame set as "results" the row is sent to the approx_wrap_2000 function
#' to be processs and then mutating result to add a new column with the ICE
#' values
#' 
#' @param ds dataset as a data.frame
#' @param nref the refernce of n count column, a string to the column name
#' @param columnname name of the column that will be used to mutates, as a string
#' @param func function that is sent through to process values
#' 
#' @return a dataframe of the original data and a new column titled after columnname
aggr_calculate_ICE <- function(ds, nref, columnname, func, limit=50){
  ds %>% filter(nref > limit) %>% rowwise %>% do({
    result = as_data_frame(.)
    result[columnname] = func(result) %>% unlist
    result
  })
}

reup <- function(x){
  x %>% as.factor %>% plyr::mapvalues(from=c(TRUE, FALSE), to=c("Increasing", "Decreasing")) %>% relevel("Increasing")
}
## nICE_2000 is a dataframe of "new" ICE value caluclated using the
## using agg
nICE_2000 <- aggr_calculate_ICE(CT_2000, "HC01_vC63", "ICE_2000", approx_wrap_2000)

## nICE_2015 is created similarly but with approx_wrap_2015
nICE_2015 <- aggr_calculate_ICE(CT_2015, "HC01_VC119", "ICE_2015", approx_wrap_2015)

ICES = data.frame(GEO.id2 = as.character(nICE_2000$trtid10), ICE_2000=nICE_2000$ICE_2000, cbsa10=nICE_2000$cbsa10) %>% inner_join(data.frame(GEO.id2=nICE_2015$GEO.id2, ICE_2015=nICE_2015$ICE_2015), by="GEO.id2")
demoICE <- ICES %>% inner_join(Demo_2000, by = "GEO.id2") %>% inner_join(Demo_2015, by="GEO.id2") %>% na.omit

overallDiff <- demoICE %>% mutate(ICE_diff=ICE_2015-ICE_2000)
wasb15 <- overallDiff %>% 
  filter(percentage_black_2000 > 15) %>% 
  group_by(group=percentage_black_2000 < percentage_black)
wasb15$group <- wasb15$group %>% reup
tob15 <- overallDiff %>% filter(percentage_black_2000 < 15 & percentage_black > 15)

summ_tob15 <- tob15 %>% summarise(mean_diff=mean(ICE_diff), mean_ICE_2000=mean(ICE_2000), mean_ICE_2015=mean(ICE_2015), n=length(GEO.id2)) 
summ_wasb15 <- wasb15 %>% summarise(mean_diff=mean(ICE_diff), mean_ICE_2000=mean(ICE_2000), mean_ICE_2015=mean(ICE_2015), n=length(GEO.id2)) 
summ_overall <- overallDiff %>% summarise(mean_diff=mean(ICE_diff), mean_ICE_2000=mean(ICE_2000), mean_ICE_2015=mean(ICE_2015), n=length(GEO.id2)) 
summ_stats <- rbind(
  overall=summ_overall, 
  was_b15_group=summ_wasb15[2,2:5], 
  was_b15_decrease=summ_wasb15[1,2:5])
by_city <- by_city_overall %>% inner_join(by_city_group, by="cbsa10") %>% inner_join(by_city_decrease, by="cbsa10") %>% inner_join(MSAs_2015, by=c("cbsa10"="GEO.id2"))

