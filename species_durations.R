rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Exploratory analysis for duration of pheno events
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Watershed Function SFA/pheno/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

#filter observations to those that have NL, FLE, FOF, and FLCC
pheno <- filter(pheno, NL >0 | FLE >0 | FOF>0 | FLCC >0)
pheno$Site <- factor(pheno$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))

#reorder dataset so that NL, FLE, FOF, and FLCC are in colum 'Event' and the values are in column 'DOY'
pheno <- pheno %>% 
  # pivot longer lets you designate columns by name so that if the order changes it's ok
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

pheno$Event<- factor(pheno$Event, levels = c("NL", "FLE", "FOF","FLCC"))

#calculate durations as max-min for each event for each species
pheno_species <- pheno %>% 
  group_by(Year, Site, Treatment, Species, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T))
#calculate deltas
pheno_species$species_duration <- pheno_species$max - pheno_species$min

#reorder dataframe
pheno_species_delta <- pheno_species %>% 
  select(-c(max, min)) %>% 
  pivot_wider(names_from = Year, values_from = species_duration)

pheno_species_delta$delta_duration <- pheno_species_delta$`2018`- pheno_species_delta$`2017`





