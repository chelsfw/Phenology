rm(list = ls())
## Date: 2021-02-18
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: day of water year snow free to end of FLE
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

greenup <- select(pheno, c(Year, Site, Elevation, Block, Treatment, Species, DOYsf, EOS, FLE, FOF))

greenup$DOWYsf <- greenup$DOYsf + 92
greenup$FLE <- greenup$FLE + 92
greenup$FOF <- greenup$FOF + 92
greenup$sf_to_fle <- greenup$FLE - greenup$DOWYsf
greenup$sf_to_fof <- greenup$FOF - greenup$DOWYsf

greenup <- greenup%>%
  group_by(Year, Site, Elevation, Block, Treatment)%>%
  summarise(sf_to_fle = mean(sf_to_fle, na.rm = T),
            sf_to_fof = mean(sf_to_fof, na.rm = T))

write.csv(greenup, "/Users/chelseawilmer/Desktop/Github/Phenology/greenup_period.csv")
