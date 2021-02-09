rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating durations
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

#filter observations to those that have a vlaue for NL, FLE, FOF, and FLCC
pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0) 

####Calculate durations as max - min date of an event####
#reorder dataset
pheno <- pheno %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#summarise for max, min, and mean by species
duration_by_species <- pheno %>% 
  group_by(Year, Site, Treatment, Species, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T),
            mean = mean(DOY, na.rm = T))

#calculate durations at species level
duration_by_species$duration <- duration_by_species$max - duration_by_species$min

#summarise for max, min, and mean by treatment
duration_by_treatment <- pheno %>% 
  group_by(Year, Site, Treatment, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T),
            mean = mean(DOY, na.rm = T))

#calculate durations at species level
duration_by_treatment$duration <- duration_by_treatment$max - duration_by_treatment$min

####Effect of year for durations####
duration_by_species <- duration_by_species %>%
  select(-c(max, min, mean))%>%
  pivot_wider(names_from = Year, values_from = duration)

duration_by_species <- na.omit(duration_by_species)

duration_by_species$effect_of_year <- duration_by_species$`2018`- duration_by_species$`2017`

#remove years colums for pivot wider
duration_by_species <- select(duration_by_species, -c("2017", "2018"))

duration_by_species <- filter(duration_by_species, Treatment == "Control")

duration_by_species$Event<- factor(duration_by_species$Event, levels = c("NL", "FLE", "FOF", "FLCC"))

duration_by_species%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect_of_year))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Montane", y = "number of days shorter (-) or longer (+)")

duration_by_species%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect_of_year))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Montane", y = "number of days shorter (-) or longer (+)")

#### effect of the treatment for durations####
duration_treatment <- duration%>%
  pivot_wider(names_from = Treatment, values_from = effect_of_year)

duration_treatment <- na.omit(duration_treatment)

#calculate difference in timing between treatments
duration_treatment$effect_of_year <- duration_treatment$Treatment - duration_treatment$Control

duration_treatment <- select(duration_treatment, -c("Control", "Treatment"))

duration_treatment%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect_of_year))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Montane", y = "number of days shorter (-) or longer (+)")

#haven't gotten back to this yet
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

