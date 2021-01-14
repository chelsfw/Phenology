rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating deltas
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

#filter observations to those that have a vlaue for NL, FLE, FOF, and FLCC
pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0) 

#kind of a mess right now
#calculate durations
duration$duration <- duration$max - duration$min

##reorder dataframe to create deltas dataset
deltas <- duration %>%
  select(-c(max, min, n, mean))%>%
  pivot_wider(names_from = Year, values_from = duration)

deltas <- na.omit(deltas)

#calculate deltas
deltas$delta_duration <- deltas$`2018`- deltas$`2017`

deltas <- deltas%>%
  select(-c(`2017`, `2018`))
