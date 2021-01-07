rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Creating files for activity period and emergence
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Watershed Function SFA/pheno/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

#filter observations to those that have a vlaue for NL, FLE, FOF, and FLCC
pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0) #playing around with difference between & and | to filter. if '&' it cuts the data down to only observations that have values for ALL phenophases that year.

#SF and activity period
pheno$activity <- pheno$FLCC-pheno$FLE
pheno$emergence <- pheno$NL-pheno$DOYsf
