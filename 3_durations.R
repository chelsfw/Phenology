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

####Calculate durations: latest date minus the earliest date####
#reorder dataset so that NL, FLE, FOF, and FLCC are in column 'Event' and the values are in column 'DOY'
duration <- pheno %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 


duration <- duration %>% 
  group_by(Year, Site, Treatment, Species, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T),
            mean = mean(DOY, na.rm = T),
            n = n())

#duration <- filter(duration, n > 1)


####Effect of year for durations####
duration <- duration %>%
  select(-c(max, min, n, mean))%>%
  pivot_wider(names_from = Year, values_from = duration)

duration <- na.omit(duration)

duration$effect_of_year <- duration$`2018`- duration$`2017`
#remove years colums for pivot wider
duration <- select(duration, -c("2017", "2018"))

duration <- filter(duration, Treatment == "Control")
duration$Event<- factor(duration$Event, levels = c("NL", "FLE", "FOF", "FLCC"))

duration%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect_of_year))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Montane", y = "number of days shorter (-) or longer (+)")

duration%>%
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

##reorder dataframe to create deltas dataset
pheno_deltas <- pheno_durations %>%
  select(-c(max, min, n, mean))%>%
  pivot_wider(names_from = Year, values_from = species_duration)

#calculate deltas
pheno_deltas$delta_duration <- pheno_deltas$`2018`-pheno_deltas$`2017`

pheno_deltas <- pheno_deltas%>%
  select(-c(`2017`, `2018`))

