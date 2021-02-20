rm(list=ls())
setwd("/Users/chelseawilmer/Desktop/Github/Phenology")
source("/Users/chelseawilmer/Desktop/Github/Phenology/gradient_microclimate_summaries.R")
library(tidyverse)
library(plyr)

pheno_microclimate <- raw.swc %>%
  group_by(Year, Site, Block, Treatment,  DOY) %>%
  dplyr::summarise(daily_mean_swc = mean(Measurement, na.rm = T))

pheno_microclimate <- filter(pheno_microclimate, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")
pheno_microclimate <- filter(pheno_microclimate, Year == 2018)

pheno_microclimate$Site <- factor(pheno_microclimate$Site, levels = c("USA", "LSA", "UM", "LM"))

                   