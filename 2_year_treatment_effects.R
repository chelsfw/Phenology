rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Creating files for duration of pheno events and change in duration of pheno events
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/1_shared_species_filter.R")

####Effect of year and year + treatment on mean date of events####
year <- pheno%>%
  group_by(Year, Site, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T),
            n = n())

year <- year%>%
  pivot_wider(names_from = Year, values_from = meanDOY)

#omit NA values after pivot wider
year <- na.omit(year)

year$effect_of_year <- year$`2018`- year$`2017`

year <- select(year, -c("2017", "2018"))

treatment <- year%>%
  pivot_wider(names_from = Treatment, values_from = effect_of_year)

treatment$effect_of_treatment <- treatment$Treatment - treatment$Control
