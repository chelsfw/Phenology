rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Creating files for duration of pheno events and change in duration of pheno events
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/1_shared_species_filter.R")

####Calculate durations and deltas as the latest date minus the earliest date####
##create durations dataset
pheno_durations <- pheno %>% 
  group_by(Year, Site, Treatment, Species, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T),
            mean = mean(DOY, na.rm = T),
            n = n())

#calculate durations
pheno_durations$species_duration <- pheno_durations$max - pheno_durations$min

##reorder dataframe to create deltas dataset
pheno_deltas <- pheno_durations %>%
  select(-c(max, min, n, mean))%>%
  pivot_wider(names_from = Year, values_from = species_duration)

#calculate deltas
pheno_deltas$delta_duration <- pheno_deltas$`2018`-pheno_deltas$`2017`

pheno_deltas <- pheno_deltas%>%
  select(-c(`2017`, `2018`))

####Write durations and deltas datasets to .csv's####
write.csv(pheno_deltas, "/Users/chelseawilmer/Desktop/Github/Phenology/deltas.csv", row.names = F) #add somet
write.csv(pheno_durations, "/Users/chelseawilmer/Desktop/Github/Phenology/durations.csv")

####Calculate durations and deltas as the difference between the later event and the earlier date####
##create durations dataset CHANGE TO MAKE EVENTS DATE OF NL-DOYsf, DATE OF FLE-NL, DATE OF FOF-FLE, DATE of FLCC-FOF
pheno_durations <- pheno %>% 
  group_by(Year, Site, Treatment, Species, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T),
            mean = mean(DOY, na.rm = T),
            n = n())

#calculate durations
pheno_durations$species_duration <- pheno_durations$max - pheno_durations$min

##reorder dataframe to create deltas dataset
pheno_deltas <- pheno_durations %>%
  select(-c(max, min, n, mean))%>%
  pivot_wider(names_from = Year, values_from = species_duration)

#calculate deltas
pheno_deltas$delta_duration <- pheno_deltas$`2018`-pheno_deltas$`2017`

pheno_deltas <- pheno_deltas%>%
  select(-c(`2017`, `2018`))

