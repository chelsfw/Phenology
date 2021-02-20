rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Processing data to make figure like Post et al. 2008
## Packages:
library(tidyverse)
library(ggpubr)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
# path for Dana
#source("Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

#filter pheno dataset for the treatment year 2018
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

treatment <- treatment%>%
  group_by(Site, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

treatment_duration <- treatment%>%
  pivot_wider(names_from = Event, values_from = meanDOY)

treatment_duration$NL <- treatment_duration$FLE - treatment_duration$NL
treatment_duration$FLE <- treatment_duration$FOF - treatment_duration$FLE
treatment_duration$FOF <- treatment_duration$FLCC - treatment_duration$FOF
treatment_duration$FLCC <- 277 - treatment_duration$FLCC

treatment_duration <- treatment_duration%>%
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "Duration") 

treatment_duration <- na.omit(treatment_duration)

timing_and_duration <- left_join(treatment_duration, treatment, by = c("Site", "Treatment", "Species", "Event"))

timing_and_duration <- filter(timing_and_duration, Event == "NL" | Event == "FLE" | Event == "FOF")

lm_erispe <- timing_and_duration%>%
  filter(Site == "LM", Species == "Erigeron speciosus")%>%
  ggplot(aes(meanDOY, Duration, color = Treatment))+
  geom_point()+ 
  geom_line()+
  labs(title = "Erigeron speciosus:LM")

um_erispe <- timing_and_duration%>%
  filter(Site == "UM", Species == "Erigeron speciosus")%>%
  ggplot(aes(meanDOY, Duration, color = Treatment))+
  geom_point()+ 
  geom_line()+
  labs(title = "Erigeron speciosus:UM")

lsa_erispe <- timing_and_duration%>%
  filter(Site == "LSA", Species == "Erigeron speciosus")%>%
  ggplot(aes(meanDOY, Duration, color = Treatment))+
  geom_point()+ 
  geom_line()+
  labs(title = "Erigeron speciosus:LSA")

ggarrange(lm_erispe, um_erispe, lsa_erispe)

lm_achmil <- timing_and_duration%>%
  filter(Site == "LM", Species == "Achillea millefolium")%>%
  ggplot(aes(meanDOY, Duration, color = Treatment))+
  geom_point()+ 
  geom_line()

lm_achnel <- timing_and_duration%>%
  filter(Site == "LM", Species == "Achnatherum nelsonii")%>%
  ggplot(aes(meanDOY, Duration, color = Treatment))+
  geom_point()+ 
  geom_line()

lm_arttri <- timing_and_duration%>%
  filter(Site == "LM", Species == "Artemisia tridentata")%>%
  ggplot(aes(meanDOY, Duration, color = Treatment))+
  geom_point()+ 
  geom_line()






